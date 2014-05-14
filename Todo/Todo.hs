module Todo.Todo
( Todo(..)
, completed
, complete
, uncomplete
, priority
, prioritised
, prioritise
, unprioritise
) where

import Data.Char (toUpper)
import System.Console.ANSI

data Todo = Todo String deriving (Eq)

instance Show Todo where
  show todo@(Todo text)
    | completed todo = coloredTodoline White
    | otherwise = case priority todo of
                    Priority 'A' -> coloredTodoline Yellow
                    Priority 'B' -> coloredTodoline Green
                    Priority 'C' -> coloredTodoline Blue
                    _ -> todoLine
    where todoLine = text
          coloredTodoline color = setColor color ++ todoLine ++ resetColor
          setColor c = setSGRCode [SetColor Foreground Dull c]
          resetColor = setSGRCode []

instance Ord Todo where
  compare t1 t2
    | completed t1 && completed t2 = compare (priority t1) (priority t2)
    | completed t1 = LT
    | completed t2 = GT
    | otherwise = compare (priority t1) (priority t2)

completed :: Todo -> Bool
completed (Todo text) =
  case text of
    ('x':' ':_) -> True
    _ -> False

complete :: Todo -> Todo
complete todo@(Todo text)
  | completed todo = todo
  | otherwise = Todo ("x " ++ text)

uncomplete :: Todo -> Todo
uncomplete todo@(Todo text)
  | completed todo = Todo (drop 2 text)
  | otherwise = todo

data Priority = None | Priority Char deriving (Eq)
instance Ord Priority where
  compare None _ = LT
  compare (Priority _) None = GT
  compare (Priority p1) (Priority p2) = flip compare p1 p2

priority :: Todo -> Priority
priority (Todo text) = -- FIXME: use regexpes...
  case priorityChar text of
    Just pri -> if pri `elem` ['A'..'E']
                  then Priority (toUpper pri)
                  else None
    Nothing -> None
  where
    priorityChar :: String -> Maybe Char
    priorityChar ('(':pri:')':_) = Just pri -- incomplete todo with priority
    priorityChar ('x':' ':'(':pri:')':_) = Just pri -- complete todo with priority
    priorityChar _ = Nothing

prioritised :: Todo -> Bool
prioritised todo = case priority todo of
                     None -> False
                     Priority _ -> True

unprioritise ::Todo -> Todo
unprioritise todo@(Todo text)
  | completed todo && prioritised todo = complete . unprioritise $ uncomplete todo
  | prioritised todo = Todo (drop 4 text)
  | otherwise = todo

prioritise :: Char -> Todo -> Todo
prioritise priorityChar todo@(Todo text)
  | completed todo = complete . prioritise priorityChar $ uncomplete todo
  | prioritised todo = prioritise priorityChar $ unprioritise todo
  | otherwise = Todo (priorityString ++ text)
  where priorityString = "(" ++ [toUpper priorityChar] ++ ") "
