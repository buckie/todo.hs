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
-- FIXME: maybe call this displayNumberedTodoList
displayTodoList :: [Todo] -> String
displayTodoList todos = unlines todoList
                     where sortedTodosWithIDs = sortBy (\(_, t1) (_, t2) -> compare t1 t2) $ todosWithIDs todos
                           showTodoID = Printf.printf "%3d "
                           todoList = [showTodoID tID ++ show todo | (tID, todo@(Todo text)) <- sortedTodosWithIDs, not $ blankLine text]
                           blankLine = ([]==) . Text.unpack . Text.strip . Text.pack

-- FIXME: dry this up (it's almost the same as the above)
displayTodos :: [Todo] -> String
displayTodos todos = unlines todoList
                     where sortedTodosWithIDs = sortBy (\(_, t1) (_, t2) -> compare t1 t2) $ todosWithIDs todos
                           todoList = [show todo | (_, todo@(Todo text)) <- sortedTodosWithIDs, not $ blankLine text]
                           blankLine = ([]==) . Text.unpack . Text.strip . Text.pack
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
    todosWithIDs' = todosWithIDs todos

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
    (removedTodosWithIDs, newTodosWithIDs) = partition (\(tID, _) -> tID `elem` targetTodoIDs) $ todosWithIDs todos
    removedTodos = map snd removedTodosWithIDs
    newTodos = map snd newTodosWithIDs

allIDsPresent :: [TodoID] -> [Todo] -> Bool
allIDsPresent tIDs todos = all (`elem` [0..length todos - 1]) tIDs

todosWithIDs :: [Todo] -> [(TodoID, Todo)]
todosWithIDs = zip [(0::Int)..]
