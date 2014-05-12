module TodoUtils
( Todo
, readTodoTxt
, serialiseTodoTxt
, displayTodoTxt
, removeTodo
, completeTodo
) where

import Data.List (partition, sort)
import Data.Char (toUpper)
import qualified Data.Text as Text

data Todo = Todo Int String deriving (Eq)

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

readTodoTxt :: String -> [Todo]
readTodoTxt todoTxt =
  zipWith Todo [0..] todoLines
  where todoLines = filter (not . blank) $ lines todoTxt
        blank = ([]==) . Text.unpack . Text.strip . Text.pack

displayTodoTxt :: [Todo] -> String
displayTodoTxt = unlines . map show . sort

serialiseTodoTxt :: [Todo] -> String
serialiseTodoTxt = unlines . map (\(Todo _ text) -> text)

removeTodo :: Int -> [Todo] -> (Maybe Todo, [Todo])
removeTodo targetTodoId todoList =
  (removedTodo, newTodoList)
  where (removedTodos, newTodoList) = partition (\(Todo tId _) -> targetTodoId == tId) todoList
        removedTodo = case removedTodos of
                          [] -> Nothing
                          (t:[]) -> Just t
                          _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId

completeTodo :: Int -> [Todo] -> (Maybe Todo, [Todo])
completeTodo targetTodoId todoList =
    (completedTodo, newTodoList)
    where (foundTodo, todoListWithoutFoundTodo) = removeTodo targetTodoId todoList
          (completedTodo, newTodoList) = case foundTodo of
                                           Just t -> (Just (complete t), todoListWithoutFoundTodo ++ [complete t])
                                           Nothing -> (Nothing, todoList)
                                           where complete todo@(Todo tId tText)
                                                     | completed todo = todo
                                                     | otherwise      = Todo tId ("x " ++ tText)


