module TodoList.Actions
( archiveTodos
, completeTodos
, uncompleteTodos
, prioritiseTodos
, unprioritiseTodos
, removeTodos
) where

import Data.List (partition)

import TodoList.Todo
import TodoList.List

type UpdateResponse = Maybe (TodoList, TodoList)
type UpdateAction = Todo -> Todo

-- fst = a TodoList containing the todos that were updated
-- snd = The new todoList (fst + unaffected todos)
partitionTodoList :: (Todo -> Bool) -> TodoList -> (TodoList, TodoList)
partitionTodoList p =
  foldl (\(left, right) listItem@(_, todo) ->
    if p todo
       then (left ++ [listItem], right)
       else (left, right ++ [listItem])
  ) ([],[])

archiveTodos :: TodoList -> UpdateResponse
archiveTodos todoList =
  case archivedTodos of
    [] -> Nothing
    _ -> Just (archivedTodos, newTodos)
  where (archivedTodos, newTodos) = partitionTodoList completed todoList

removeTodos :: [TodoID] -> TodoList -> UpdateResponse
removeTodos targetTodoIDs todoList =
  if canUpdate targetTodoIDs todoList
     then Just (removedTodosWithIDs, newTodosWithIDs)
     else Nothing
  where
    (removedTodosWithIDs, newTodosWithIDs) = partition (\(tID, _) -> tID `elem` targetTodoIDs) todoList

updateTodos :: [TodoID] -> TodoList -> UpdateAction -> UpdateResponse
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

prioritiseTodos :: Char -> [TodoID] -> TodoList -> UpdateResponse
prioritiseTodos priorityInput targetTodoIDs todos =
  updateTodos targetTodoIDs todos (prioritise priorityInput)

unprioritiseTodos :: [TodoID] -> TodoList -> UpdateResponse
unprioritiseTodos targetTodoIDs todos = updateTodos targetTodoIDs todos unprioritise

completeTodos :: [TodoID] -> TodoList -> UpdateResponse
completeTodos targetTodoIDs todos = updateTodos targetTodoIDs todos complete

uncompleteTodos :: [TodoID] -> TodoList -> UpdateResponse
uncompleteTodos targetTodoIDs todos = updateTodos targetTodoIDs todos uncomplete

canUpdate :: [TodoID] -> TodoList -> Bool
canUpdate targetTodoIDs todoList =
  all (`elem` availableTodoIDs) targetTodoIDs
  where availableTodoIDs = map fst todoList
