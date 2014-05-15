module Todo.Actions -- FIXME: all these act on a todo list...
( archiveTodos
, completeTodos
, uncompleteTodos
, prioritiseTodos
, unprioritiseTodos
, removeTodos
) where

import Data.List (partition)

import Todo.Todo
import Todo.List

type AffectedTodo = Todo
type UpdateResponse = Maybe ([AffectedTodo], [Todo])

archiveTodos :: [Todo] -> UpdateResponse
archiveTodos todos =
  case archivedTodos of
    [] -> Nothing
    _ -> Just (archivedTodos, newTodos)
  where (archivedTodos, newTodos) = partition completed todos

removeTodos :: [TodoID] -> [Todo] -> UpdateResponse
removeTodos targetTodoIDs todos =
  if canUpdate targetTodoIDs todos
     then Just (removedTodos, newTodos)
     else Nothing
  where
    (removedTodosWithIDs, newTodosWithIDs) = partition (\(tID, _) -> tID `elem` targetTodoIDs) $ allTodosWithIDs todos
    removedTodos = map snd removedTodosWithIDs
    newTodos = map snd newTodosWithIDs

type UpdateAction = Todo -> AffectedTodo
updateTodos :: [TodoID] -> [Todo] -> UpdateAction -> UpdateResponse
updateTodos targetTodoIDs todos updateF =
  if canUpdate targetTodoIDs todos
    then Just (onlyUpdatedTodos, newTodos)
    else Nothing
  where
    (onlyUpdatedTodos, newTodos) = foldl parseTodos ([],[]) todosWithIDs'
    parseTodos (onlyUpdatedTodos', newTodos') todoWithID
      | needsUpdate todoWithID = (onlyUpdatedTodos' ++ [updatedTodo], newTodos' ++ [updatedTodo])
      | otherwise = (onlyUpdatedTodos', newTodos' ++ [oldTodo])
      where
        updatedTodo = updateF $ snd todoWithID
        oldTodo = snd todoWithID
    needsUpdate (tID, _) = tID `elem` targetTodoIDs
    todosWithIDs' = allTodosWithIDs todos

prioritiseTodos :: Char -> [TodoID] -> [Todo] -> UpdateResponse
prioritiseTodos priorityInput targetTodoIDs todos =
  updateTodos targetTodoIDs todos (prioritise priorityInput)

unprioritiseTodos :: [TodoID] -> [Todo] -> UpdateResponse
unprioritiseTodos targetTodoIDs todos = updateTodos targetTodoIDs todos unprioritise

completeTodos :: [TodoID] -> [Todo] -> UpdateResponse
completeTodos targetTodoIDs todos = updateTodos targetTodoIDs todos complete

uncompleteTodos :: [TodoID] -> [Todo] -> UpdateResponse
uncompleteTodos targetTodoIDs todos = updateTodos targetTodoIDs todos uncomplete

allIDsPresent :: [TodoID] -> [Todo] -> Bool
allIDsPresent tIDs todos = all (`elem` [0..length todos - 1]) tIDs

canUpdate :: [TodoID] -> [Todo] -> Bool
-- FIXME: make sure that none of the IDs aren't for "blank" todos either
canUpdate = allIDsPresent
