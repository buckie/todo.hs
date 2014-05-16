module TodoList.Todo
( Todo(..)
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
import Data.Maybe (fromMaybe)

import TodoList.Utils

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
  | completed todo = Todo (drop completionStrLength text)
  | otherwise = todo
  where completionStrLength = 2

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
  | completed todo = complete . unprioritise $ uncomplete todo
  | prioritised todo = Todo (drop priorityStrLength text)
  | otherwise = todo
  where priorityStrLength = 4

prioritise :: Char -> Todo -> Todo
prioritise priorityInput todo@(Todo text)
  | completed todo = complete . prioritise priorityInput $ uncomplete todo
  | prioritised todo = prioritise priorityInput $ unprioritise todo
  | otherwise = Todo $ fromMaybe "" priorityString ++ text
  -- FIXME: the validprioritychar check should be in the CLI interface module,
  -- so we can give a helpful error message...
  where priorityString = if validPriorityChar priorityInput
                            then Just $ "(" ++ [toUpper priorityInput] ++ ") "
                            else Nothing

priorityChar :: Todo -> Maybe Char
priorityChar todo@(Todo text)
  | completed todo = priorityChar $ uncomplete todo
  | otherwise = case text of
                  ('(':pri:')':' ':_) -> if validPriorityChar pri
                                           then Just (toUpper pri)
                                           else Nothing
                  _ -> Nothing

validPriorityChar :: Char -> Bool
-- FIXME: it feels like this priorityChar stuff should really just be in
-- the serialisation logic..
validPriorityChar = (`elem` ['A'..'Z']) . toUpper
