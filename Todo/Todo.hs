module Todo.Todo
( Todo(..)
, blank
, completed
, complete
, uncomplete
, priority
, priorityChar
, prioritised
, prioritise
, unprioritise
) where

import Data.Char (toUpper)
import qualified Data.Text as Text

import Todo.Utils
import System.Console.ANSI

data Todo = Todo String deriving (Eq)

instance Show Todo where
  show todo@(Todo text)
    | completed todo = colouredStr White text
    | otherwise = case priority todo of
                    Priority 'A' -> colouredStr Yellow text
                    Priority 'B' -> colouredStr Green text
                    Priority 'C' -> colouredStr Blue text
                    _ -> text

instance Ord Todo where
  compare t1 t2
    | completed t1 && completed t2 = compare (priority t1) (priority t2)
    | completed t1 = LT
    | completed t2 = GT
    | otherwise = compare (priority t1) (priority t2)

blank :: Todo -> Bool
blank (Todo text) =
  blankLine text
  where
    blankLine = ([]==) . Text.unpack . Text.strip . Text.pack

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
priority todo =
  case priorityChar todo of
    Nothing -> None
    Just pri -> if pri `elem` ['A'..'E']
                  then Priority pri
                  else None

prioritised :: Todo -> Bool
prioritised todo
  | completed todo = prioritised $ uncomplete todo
  | otherwise = case priorityChar todo of
                  Nothing -> False
                  Just _ -> True

unprioritise ::Todo -> Todo
unprioritise todo@(Todo text)
  | completed todo && prioritised todo = complete . unprioritise $ uncomplete todo
  | prioritised todo = Todo (drop 4 text)
  | otherwise = todo

prioritise :: Char -> Todo -> Todo
prioritise priorityInput todo@(Todo text)
  | completed todo = complete . prioritise priorityInput $ uncomplete todo
  | prioritised todo = prioritise priorityInput $ unprioritise todo
  | otherwise = Todo (priorityString ++ text)
  where priorityString = "(" ++ [toUpper priorityInput] ++ ") "

priorityChar :: Todo -> Maybe Char
priorityChar todo@(Todo text)
  | completed todo = priorityChar $ uncomplete todo
  | otherwise = case text of
                  -- FIXME: use regexpes...
                  ('(':pri:')':' ':_) -> if toUpper pri `elem` ['A'..'E']
                                           then Just (toUpper pri)
                                           else Nothing
                  _ -> Nothing

