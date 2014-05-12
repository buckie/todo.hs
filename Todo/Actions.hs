module Todo.Actions
( Todo
, readTodoTxt
, serialiseTodoTxt
, displayTodoTxt
, removeTodo
, completeTodo
) where

import Data.List (partition, sort)
import qualified Data.Text as Text

import Todo.Todo

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
