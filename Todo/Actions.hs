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

import Data.List (sort)
import qualified Data.Text as Text

import Todo.Todo
type TodoID = Int

readTodos :: String -> [Todo]
-- FIXME: If we want ot keep the white line in the file we could put the
-- filter on the display
-- (I often go in and edit the todo.txt file by hand, leaving space for clarity
-- It'd be nice if that was mantained across saves..)
readTodos todoTxt =
  map Todo todoLines
  where todoLines = filter (not . blank) $ lines todoTxt
        blank = ([]==) . Text.unpack . Text.strip . Text.pack

displayTodos :: [Todo] -> String
displayTodos = unlines . zipWith ((++) . show) [(0::Int)..] . map show . sort

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo text) -> text) . sort

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
