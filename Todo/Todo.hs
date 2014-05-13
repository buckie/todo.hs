module Todo.Todo
( Todo(..)
, TodoId
, completed
, priority
) where

import Data.Char (toUpper)

type TodoId = Int
data Todo = Todo TodoId String deriving (Eq)

instance Show Todo where
  show (Todo todoId todoText) = show todoId ++ " " ++ todoText

instance Ord Todo where
  compare t1 t2
    | completed t1 && completed t2 = compare (priority t1) (priority t2)
    | completed t1 = LT
    | completed t2 = GT
    | otherwise = compare (priority t1) (priority t2)

completed :: Todo -> Bool
completed (Todo _ text) =
  case text of
    ('x':' ':_) -> True
    _ -> False

data Priority = None | Priority Char deriving (Eq)

instance Ord Priority where
  compare None _ = LT
  compare (Priority _) None = GT
  compare (Priority p1) (Priority p2) = flip compare p1 p2

priority :: Todo -> Priority
priority (Todo _ todoText) = -- FIXME: use regexpes...
  case priorityChar todoText of
    Just pri -> if pri `elem` ['A'..'E']
                  then Priority (toUpper pri)
                  else None
    Nothing -> None
  where
    priorityChar :: String -> Maybe Char
    priorityChar ('(':pri:')':_) = Just pri -- incomplete todo with priority
    priorityChar ('x':' ':'(':pri:')':_) = Just pri -- complete todo with priority
    priorityChar _ = Nothing
