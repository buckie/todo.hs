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

readTodos :: String -> [Todo]
readTodos todoTxt =
  zipWith Todo [0..] todoLines
  where todoLines = filter (not . blank) $ lines todoTxt
        blank = ([]==) . Text.unpack . Text.strip . Text.pack

displayTodos :: [Todo] -> String
displayTodos = unlines . map show . sort

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo _ text) -> text)

type UpdateMessage = String
type UpdatedTodo = Todo
type TodosUpdater = TodoId -> [Todo] -> ([UpdateMessage], Maybe [UpdatedTodo])

-- FIXME: removeTodo & completeTodo are SOAKED (i.e., not DRY)
removeTodo :: TodosUpdater
removeTodo targetTodoId todoList =
  ([updateMessage], maybeNewTodoList)
  where (removedTodos, newTodoList) = partition (\(Todo tId _) -> targetTodoId == tId) todoList
        (updateMessage, maybeNewTodoList) = case removedTodos of
                                              [] -> ("Could not find todo #" ++ show targetTodoId, Nothing)
                                              (_:[]) -> ("Todo #" ++ show targetTodoId ++ " removed", Just newTodoList)
                                              _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId

completeTodo :: TodosUpdater
completeTodo targetTodoId todoList =
    ([updateMessage], maybeNewTodoList)
    where (todosToUpdate, todoListWithoutFoundTodo) = partition (\(Todo tId _) -> targetTodoId == tId) todoList
          (updateMessage, maybeNewTodoList) = case todosToUpdate of
                                                [] -> ("Could not find todo #" ++ show targetTodoId, Nothing)
                                                (t:[])-> ( "Todo #" ++ show targetTodoId ++ " completed"
                                                          , Just $ todoListWithoutFoundTodo ++ [complete t])
                                                _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId
                                                where complete todo@(Todo tId tText)
                                                          | completed todo = todo
                                                          | otherwise      = Todo tId ("x " ++ tText)
