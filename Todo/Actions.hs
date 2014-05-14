module Todo.Actions
( Todo
, TodoID
, readTodos
, serialiseTodos
, displayTodos
, completeTodos
, uncompleteTodos
, prioritiseTodos
, unprioritiseTodos
, removeTodos
) where

import Data.List (sortBy)
import qualified Data.Text as Text

import Todo.Todo
type TodoID = Int

readTodos :: String -> [Todo]
readTodos todoTxt =
  map Todo $ lines todoTxt

displayTodos :: [Todo] -> String
-- FIXME: show numbers with a fixed "width"
displayTodos todos = unlines todoList
                     where sortedTodosWithIDs = sortBy (\(_, t1) (_, t2) -> compare t1 t2) $ todosWithIDs todos
                           todoList = [show tID ++ " " ++ show todo | (tID, todo@(Todo text)) <- sortedTodosWithIDs, not $ blankLine text]
                           blankLine = ([]==) . Text.unpack . Text.strip . Text.pack

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo text) -> text)

type UpdatedTodo = Todo
type UpdateAction = Todo -> UpdatedTodo
-- FIXME: should return Just ([UpdatedTodo], [Todo])
--        so that we can then give more info in todos affected
updateTodos :: [TodoID] -> [Todo] -> UpdateAction -> Maybe [UpdatedTodo]
updateTodos targetTodoIDs todos updateF =
  if allIDsPresent targetTodoIDs todos
    then Just $ map updateIfneeded $ todosWithIDs todos
    else Nothing
  where
    updateIfneeded (tID, todo) = if tID `elem` targetTodoIDs
                                    then updateF todo
                                    else todo


prioritiseTodos :: Char -> [TodoID] -> [Todo] -> Maybe [UpdatedTodo]
prioritiseTodos priorityChar targetTodoIDs todos =
  updateTodos targetTodoIDs todos (prioritise priorityChar)

unprioritiseTodos :: [TodoID] -> [Todo] -> Maybe [UpdatedTodo]
unprioritiseTodos targetTodoIDs todos = updateTodos targetTodoIDs todos unprioritise

completeTodos :: [TodoID] -> [Todo] -> Maybe [UpdatedTodo]
completeTodos targetTodoIDs todos = updateTodos targetTodoIDs todos complete

uncompleteTodos :: [TodoID] -> [Todo] -> Maybe [UpdatedTodo]
uncompleteTodos targetTodoIDs todos = updateTodos targetTodoIDs todos uncomplete

removeTodos :: [TodoID] -> [Todo] -> Maybe [Todo]
removeTodos targetTodoIDs todos =
  if allIDsPresent targetTodoIDs todos
     then Just updatedTodos
     else Nothing
  where
    updatedTodos = [todo | (tID, todo) <- todosWithIDs todos, tID `elem` targetTodoIDs]

allIDsPresent :: [TodoID] -> [Todo] -> Bool
allIDsPresent tIDs todos = all (`elem` [0..length todos - 1]) tIDs

todosWithIDs :: [Todo] -> [(TodoID, Todo)]
todosWithIDs = zip [0..]
