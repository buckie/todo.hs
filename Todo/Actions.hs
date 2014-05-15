module Todo.Actions
( Todo
, TodoID
, readTodos
, serialiseTodos
, displayTodoList
, displayTodos
, archiveTodos
, completeTodos
, uncompleteTodos
, prioritiseTodos
, unprioritiseTodos
, removeTodos
) where

import Data.List (sortBy, partition)
import qualified Data.Text as Text
import qualified Text.Printf as Printf

import Todo.Todo
type TodoID = Int

-- ###############################################################
-- FIXME: split into Todo.Marshalling
readTodos :: String -> [Todo]
-- FIXME: maybe this is not the right place .. but priority needs to be
-- uppercased; even if I the input to this fn is lowercase
readTodos todoTxt = map Todo $ lines todoTxt

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo text) -> text)
-- ###############################################################


-- ###############################################################
-- FIXME: split into Todo.Show

displayTodoList :: [Todo] -> String
displayTodoList todos = unlines [showTodoID tID ++ show todo | (tID, todo) <- todoList todos]
                        where showTodoID = Printf.printf "%3d "

displayTodos :: [Todo] -> String
displayTodos todos = unlines [ show todo | (_, todo) <- todoList todos]

-- ###############################################################


type ArchivedTodo = Todo
archiveTodos :: [Todo] -> Maybe ([ArchivedTodo], [Todo])
archiveTodos todos =
  case archivedTodos of
    [] -> Nothing
    _ -> Just (archivedTodos, newTodos)
  where (archivedTodos, newTodos) = partition completed todos

type UpdatedTodo = Todo
type UpdateAction = Todo -> UpdatedTodo
updateTodos :: [TodoID] -> [Todo] -> UpdateAction -> Maybe ([UpdatedTodo], [Todo])
updateTodos targetTodoIDs todos updateF =
  if canUpdate
    then Just (onlyUpdatedTodos, newTodos)
    else Nothing
  where
    canUpdate = allIDsPresent targetTodoIDs todos

    (onlyUpdatedTodos, newTodos) = foldl parseTodos ([],[]) todosWithIDs'

    parseTodos (onlyUpdatedTodos', newTodos') todoWithID
      | needsUpdate todoWithID = (onlyUpdatedTodos' ++ [updatedTodo], newTodos' ++ [updatedTodo])
      | otherwise = (onlyUpdatedTodos', newTodos' ++ [oldTodo])
      where
        updatedTodo = updateF $ snd todoWithID
        oldTodo = snd todoWithID

    needsUpdate (tID, _) = tID `elem` targetTodoIDs
    todosWithIDs' = allTodosWithIDs todos

-- FIXME: type UpdateResponse = Maybe ([UpdatedTodo], [Todo])
-- and refactor
prioritiseTodos :: Char -> [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
prioritiseTodos priorityChar targetTodoIDs todos =
  updateTodos targetTodoIDs todos (prioritise priorityChar)

unprioritiseTodos :: [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
unprioritiseTodos targetTodoIDs todos = updateTodos targetTodoIDs todos unprioritise

completeTodos :: [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
completeTodos targetTodoIDs todos = updateTodos targetTodoIDs todos complete

uncompleteTodos :: [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
uncompleteTodos targetTodoIDs todos = updateTodos targetTodoIDs todos uncomplete

type RemovedTodo = Todo
removeTodos :: [TodoID] -> [Todo] -> Maybe ([RemovedTodo], [Todo])
removeTodos targetTodoIDs todos =
  if allIDsPresent targetTodoIDs todos
     then Just (removedTodos, newTodos)
     else Nothing
  where
    (removedTodosWithIDs, newTodosWithIDs) = partition (\(tID, _) -> tID `elem` targetTodoIDs) $ allTodosWithIDs todos
    removedTodos = map snd removedTodosWithIDs
    newTodos = map snd newTodosWithIDs

allIDsPresent :: [TodoID] -> [Todo] -> Bool
allIDsPresent tIDs todos = all (`elem` [0..length todos - 1]) tIDs

allTodosWithIDs :: [Todo] -> [(TodoID, Todo)]
allTodosWithIDs = zip [(0::Int)..]

todoList :: [Todo] -> [(TodoID, Todo)]
todoList todos = filter (\(_, Todo tText) -> not $ blank tText) sortedTodosWithIDs
                 where sortedTodosWithIDs = sortBy (\(_, t1) (_, t2) -> compare t1 t2) $ allTodosWithIDs todos
                       blank = ([]==) . Text.unpack . Text.strip . Text.pack
