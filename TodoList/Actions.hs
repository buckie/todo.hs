module TodoList.Actions
( TargetTodoIDs
, TodoListUpdateAction
, TodoListUpdateResponse
, prioritiseTodos
, unprioritiseTodos
, completeTodos
, uncompleteTodos
, appendToTodos
, prependToTodos
, removeTodos
, archiveTodoList
) where

import Data.List (partition)

import TodoList.Todo
import TodoList.List

type TargetTodoIDs = [TodoID]
type TodoListUpdateAction = [TodoID] -> TodoList -> TodoListUpdateResponse
type TodoListUpdateResponse = Maybe (TodoList, TodoList)

completeTodos :: TodoListUpdateAction
completeTodos targetTodoIDs todos = updateTodos targetTodoIDs todos complete

uncompleteTodos :: TodoListUpdateAction
uncompleteTodos targetTodoIDs todos = updateTodos targetTodoIDs todos uncomplete

prioritiseTodos :: Char -> TodoListUpdateAction
prioritiseTodos priorityInput targetTodoIDs todos =
  updateTodos targetTodoIDs todos (prioritise priorityInput)

unprioritiseTodos :: TodoListUpdateAction
unprioritiseTodos targetTodoIDs todos = updateTodos targetTodoIDs todos unprioritise

appendToTodos :: String -> TodoListUpdateAction
appendToTodos textToAppend targetTodoIDs todos = updateTodos targetTodoIDs todos (append textToAppend)

prependToTodos :: String -> TodoListUpdateAction
prependToTodos textToPrepend targetTodoIDs todos = updateTodos targetTodoIDs todos (prepend textToPrepend)

removeTodos :: TodoListUpdateAction
removeTodos targetTodoIDs todoList =
  if canUpdate targetTodoIDs todoList
     then Just (removedTodosWithIDs, newTodosWithIDs)
     else Nothing
  where
    (removedTodosWithIDs, newTodosWithIDs) = partition (\(tID, _) -> tID `elem` targetTodoIDs) todoList

archiveTodoList :: TodoList -> TodoListUpdateResponse
-- TODO: (A) add completion date
archiveTodoList todoList =
  case archivedTodos of
    [] -> Nothing
    _ -> Just (compactTodoList archivedTodos, newTodos)
  where (archivedTodos, newTodos) = partitionTodoList completed todoList

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

updateTodos :: TargetTodoIDs -> TodoList -> TodoUpdateAction -> TodoListUpdateResponse
updateTodos targetTodoIDs todoList updateF =
  if canUpdate targetTodoIDs todoList
    then Just (onlyUpdated, everything)
    else Nothing
  where
    (onlyUpdated, everything) = foldl updateIfNeeded ([],[]) todoList
    updateIfNeeded (onlyUpdated', everything') listItem
      | needsUpdate listItem = (onlyUpdated' ++ [update listItem], everything' ++ [update listItem])
      | otherwise = (onlyUpdated', everything' ++ [listItem])
      where
        update (tID, todo)= (tID, updateF todo)
        needsUpdate (tID, _) = tID `elem` targetTodoIDs

compactTodoList :: TodoList -> TodoList
compactTodoList = zipWith (\tID (_, todo) -> (tID, todo)) [(1::Int)..]

canUpdate :: TargetTodoIDs -> TodoList -> Bool
canUpdate targetTodoIDs todoList =
  all (`elem` availableTodoIDs) targetTodoIDs
  where availableTodoIDs = map fst todoList

