module Todo.Actions
( Todo
, TodoId
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
type UpdateResponse = ([UpdateMessage], Maybe [UpdatedTodo])

findAndUpdateTodos :: TodoId -> [Todo] -> ([Todo] -> ([UpdateMessage], [UpdatedTodo])) -> UpdateResponse
findAndUpdateTodos targetTodoId todoList f =
    (updateMessages, maybeNewTodos)
    where (todosToUpdate, otherTodos) = partition (\(Todo tId _) -> targetTodoId == tId) todoList
          (updateMessages, maybeNewTodos) = case todosToUpdate of
                                                [] -> (["Could not find todo #" ++ show targetTodoId], Nothing)
                                                (_:[]) -> let (updateMessages', updatedTodos) = f todosToUpdate in
                                                             (updateMessages', Just $ otherTodos ++ updatedTodos)
                                                _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId

removeTodo :: TodoId -> [Todo] -> UpdateResponse
removeTodo targetTodoId todoList = findAndUpdateTodos targetTodoId todoList remove
                                   where remove :: [Todo] -> ([UpdateMessage], [UpdatedTodo])
                                         remove ts = (map (\(Todo tId _) -> "Todo #" ++ show tId ++ " removed.") ts, [])

completeTodo :: TodoId -> [Todo] -> UpdateResponse
completeTodo targetTodoId todoList = findAndUpdateTodos targetTodoId todoList complete
                                     where -- complete :: [Todo] -> ([UpdateMessage], [UpdatedTodo])
                                           complete ts = ( map (\(Todo tId _) -> show tId ++ " updated.") ts
                                                         , map (\(Todo tId text) -> (Todo tId ("x " ++ text))) ts)

