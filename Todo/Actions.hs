module Todo.Actions
( Todo
, TodoId
, readTodos
, serialiseTodos
, displayTodos
, completeTodos
, uncompleteTodos
, prioritiseTodos
, unprioritiseTodos
, removeTodos
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
-- FIXME: should return Maybe ([UpdatedTodo], [Todo])
--        so that we can then give more info in todos affected
updateTodos :: [TodoId] -> [Todo] -> UpdateAction -> Maybe [UpdatedTodo]
updateTodos targetTodoIds todos updateF =
  case updatedTodos of
    [] -> Nothing
    -- FIXME: should probably recalculate todo IDs at this point since the
    --        list is all scrambled up
    _ -> Just $ otherTodos ++ updatedTodos
  where
    (todosToUpdate, otherTodos) = findTodos targetTodoIds todos
    updatedTodos = map updateF todosToUpdate

prioritiseTodos :: Char -> [TodoId] -> [Todo] -> Maybe [UpdatedTodo]
prioritiseTodos priorityChar targetTodoIds todos =
  updateTodos targetTodoIds todos (prioritise priorityChar)

unprioritiseTodos :: [TodoId] -> [Todo] -> Maybe [UpdatedTodo]
unprioritiseTodos targetTodoIds todos = updateTodos targetTodoIds todos unprioritise

completeTodos :: [TodoId] -> [Todo] -> Maybe [UpdatedTodo]
completeTodos targetTodoIds todos = updateTodos targetTodoIds todos complete

uncompleteTodos :: [TodoId] -> [Todo] -> Maybe [UpdatedTodo]
uncompleteTodos targetTodoIds todos = updateTodos targetTodoIds todos uncomplete

removeTodos :: [TodoId] -> [Todo] -> Maybe [Todo]
removeTodos targetTodoIds todos =
  case removedTodos of
    [] -> Nothing
    _ -> Just otherTodos
  where
    (removedTodos, otherTodos) = findTodos targetTodoIds todos

