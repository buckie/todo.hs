module Todo
( Todo(..)

, complete
, uncomplete
, prioritise
, unprioritise
, prepend
, append

, completed
, prioritised
, priority

) where

import           Data.Char    (toUpper)
import           Data.Maybe   (fromMaybe)

import           List.Actions
import           Utils

data Todo = Todo String deriving (Eq)


data Priority = None
              | Priority Char deriving (Eq)

-------------------------------------------------------------------------------
-- Todo Action things
-------------------------------------------------------------------------------

complete :: Todo -> Result Todo
complete todo@(Todo text)
  | completed todo = Unchanged todo
  | otherwise = Updated $ Todo ("x " ++ text)

uncomplete :: Todo -> Result Todo
uncomplete todo@(Todo text)
  | completed todo = Updated $ Todo (drop completionStrLength text)
  | otherwise = Unchanged todo
  where completionStrLength = 2

unprioritise :: Todo -> Result Todo
unprioritise todo@(Todo text)
  | completed todo = complete . (fromResult . unprioritise) . fromResult $ uncomplete todo
  | prioritised todo = Updated $ Todo (drop priorityStrLength text)
  | otherwise = Unchanged todo
  where priorityStrLength = 4


func :: Priority
func arguments =
  case arguments of


prioritise :: Char -> Todo -> Result Todo
prioritise priorityInput todo@(Todo text)
  | completed todo = complete . (fromResult . prioritise priorityInput) . fromResult $ uncomplete todo
  | prioritised todo = prioritise priorityInput . fromResult $ unprioritise todo
  | otherwise = Updated $ Todo $ fromMaybe "" priorityString ++ text
  where priorityString = if validPriorityChar priorityInput
                            then Just $ "(" ++ [toUpper priorityInput] ++ ") "
                            else Nothing

prepend :: String -> Todo -> Result Todo
prepend textToPrepend (Todo tText) = Updated $ Todo $ textToPrepend ++ " " ++ tText

append :: String -> Todo -> Result Todo
append textToAppend (Todo tText) = Updated $ Todo $ tText ++ " " ++ textToAppend

-------------------------------------------------------------------------------
-- Todo Predicate things
-------------------------------------------------------------------------------
completed :: Todo -> Bool
completed (Todo text) =
  case text of
    ('x':' ':_) -> True
    _ -> False

priority :: Todo -> Priority
priority todo =
  case priorityChar todo of
    Nothing -> None
    Just pri -> if pri `elem` ['A'..'E']
                  then Priority pri
                  else None

prioritised :: Todo -> Bool
prioritised todo
  | completed todo = prioritised $ fromResult $ uncomplete todo
  | otherwise = case priorityChar todo of
                  Nothing -> False
                  Just _ -> True


-------------------------------------------------------------------------------
-- Instances & things
-------------------------------------------------------------------------------
instance Show Todo where

  show todo@(Todo text)
    | completed todo = colouredStr Black text
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

instance Ord Priority where
  compare None _ = LT
  compare (Priority _) None = GT
  compare (Priority p1) (Priority p2) = flip compare p1 p2


-------------------------------------------------------------------------------
-- Helper things
-------------------------------------------------------------------------------
priorityChar :: Todo -> Maybe Char
priorityChar todo@(Todo text)
  | completed todo = priorityChar . fromResult $ uncomplete todo
  | otherwise = case text of
                  ('(':pri:')':' ':_) -> if validPriorityChar pri
                                           then Just (toUpper pri)
                                           else Nothing
                  _ -> Nothing

validPriorityChar :: Char -> Bool
validPriorityChar = (`elem` ['A'..'Z']) . toUpper
