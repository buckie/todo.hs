module Todo.Actions
( Todo
, TodoId
, readTodos
, serialiseTodos
, displayTodos
, removeTodos
, completeTodos
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
serialiseTodos = unlines . map (\(Todo _ text) -> text) . sort

type FoundTodo = Todo
type OtherTodo = Todo
findTodos :: [TodoId] -> [Todo] -> ([FoundTodo], [OtherTodo])
findTodos targetTodoIds todos = (foundTodos, otherTodos)
                                where
                                  (foundTodos, otherTodos) = partition (\(Todo tId _) -> tId `elem` targetTodoIds) todos

type UpdatedTodo = Todo
type UpdateAction = Todo -> UpdatedTodo
updateTodos :: [TodoId] -> [Todo] -> UpdateAction -> Maybe [UpdatedTodo]
updateTodos targetTodoIds todos updateF =
  case updatedTodos of
    [] -> Nothing
    _ -> Just $ otherTodos ++ updatedTodos
  where
    (todosToUpdate, otherTodos) = findTodos targetTodoIds todos
    updatedTodos = map updateF todosToUpdate

completeTodos :: [TodoId] -> [Todo] -> Maybe [UpdatedTodo]
completeTodos targetTodoIds todos = updateTodos targetTodoIds todos complete

removeTodos :: [TodoId] -> [Todo] -> Maybe [Todo]
removeTodos targetTodoIds todos =
  case removedTodos of
    [] -> Nothing
    _ -> Just otherTodos
  where
    (removedTodos, otherTodos) = findTodos targetTodoIds todos

