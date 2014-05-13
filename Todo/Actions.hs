module Todo.Actions
( Todo
, TodoId
, TodosUpdater
, readTodos
, serialiseTodos
, displayTodos
, removeTodo
, completeTodo
) where

import Data.List (partition, sort)
import qualified Data.Text as Text

import Todo.Todo
type TodosUpdater = TodoId -> [Todo] -> (Maybe Todo, [Todo])

readTodos :: String -> [Todo]
readTodos todoTxt =
  zipWith Todo [0..] todoLines
  where todoLines = filter (not . blank) $ lines todoTxt
        blank = ([]==) . Text.unpack . Text.strip . Text.pack

displayTodos :: [Todo] -> String
displayTodos = unlines . map show . sort

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo _ text) -> text)

removeTodo :: TodosUpdater
removeTodo targetTodoId todoList =
  (removedTodo, newTodoList)
  where (removedTodos, newTodoList) = partition (\(Todo tId _) -> targetTodoId == tId) todoList
        removedTodo = case removedTodos of
                          [] -> Nothing
                          (t:[]) -> Just t
                          _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId

completeTodo :: TodosUpdater
completeTodo targetTodoId todoList =
    (completedTodo, newTodoList)
    where (foundTodo, todoListWithoutFoundTodo) = removeTodo targetTodoId todoList
          (completedTodo, newTodoList) = case foundTodo of
                                           Just t -> (Just (complete t), todoListWithoutFoundTodo ++ [complete t])
                                           Nothing -> (Nothing, todoList)
                                           where complete todo@(Todo tId tText)
                                                     | completed todo = todo
                                                     | otherwise      = Todo tId ("x " ++ tText)
