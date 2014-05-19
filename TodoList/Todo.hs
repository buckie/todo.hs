module TodoList.Todo
( Todo(..)
, TodoUpdateAction
, completed
, complete
, uncomplete
, priority
, prioritised
, prioritise
, unprioritise
, prepend
, append
, normalise
) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import TodoList.Utils

data Todo = Todo String deriving (Eq)

-- FIXME: these should really be Either ErrorMessage UpdatedTodo
-- so that the messages can be forwarded to the interface
-- and so that we can avoid useless saves to file
type TodoUpdateAction = Todo -> Todo

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

complete :: TodoUpdateAction
complete todo@(Todo text)
  | completed todo = todo
  | otherwise = Todo ("x " ++ text)

uncomplete :: TodoUpdateAction
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

unprioritise :: TodoUpdateAction
unprioritise todo@(Todo text)
  | completed todo = complete . unprioritise $ uncomplete todo
  | prioritised todo = Todo (drop priorityStrLength text)
  | otherwise = todo
  where priorityStrLength = 4

prioritise :: Char -> TodoUpdateAction
prioritise priorityInput todo@(Todo text)
  | completed todo = complete . prioritise priorityInput $ uncomplete todo
  | prioritised todo = prioritise priorityInput $ unprioritise todo
  | otherwise = Todo $ fromMaybe "" priorityString ++ text
  where priorityString = if validPriorityChar priorityInput
                            then Just $ "(" ++ [toUpper priorityInput] ++ ") "
                            else Nothing

prepend :: String -> TodoUpdateAction
prepend textToPrepend (Todo tText) = Todo $ textToPrepend ++ " " ++ tText

append :: String -> TodoUpdateAction
append textToAppend (Todo tText) = Todo $ tText ++ " " ++ textToAppend

normalise :: TodoUpdateAction
normalise todo
  | completed todo = complete . normalise $ uncomplete todo
  | prioritised todo = prioritise priorityChar' $ unprioritise todo
  | otherwise = todo
  where priorityChar' = fromMaybe 'A' $ priorityChar todo

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

priorityChar :: Todo -> Maybe Char
priorityChar todo@(Todo text)
  | completed todo = priorityChar $ uncomplete todo
  | otherwise = case text of
                  ('(':pri:')':' ':_) -> if validPriorityChar pri
                                           then Just (toUpper pri)
                                           else Nothing
                  _ -> Nothing

validPriorityChar :: Char -> Bool
validPriorityChar = (`elem` ['A'..'Z']) . toUpper
