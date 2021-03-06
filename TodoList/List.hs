module TodoList.List
( TodoID
, TodoList
, sortTodoList
, displayTodoList
, displayOnlyTodos
) where

import qualified Text.Printf as Printf
import Data.List (sortBy)

import TodoList.Todo

type TodoID = Int
-- TODO: refactor: newtype "ListItem": ListItem (TodoID, Todo) HOW?
-- TODO: instance Functor ListItem where..
-- TODO: instance Eq ListItem where..
-- TODO: instance Ord ListItem where..
-- TODO: instance Show ListItem?
type TodoList = [(TodoID, Todo)]

sortTodoList :: TodoList -> TodoList
sortTodoList = sortBy (\(_, t1) (_, t2) -> compare t1 t2)

displayTodoList :: TodoList -> String
displayTodoList todoList = unlines [showTodoID tID ++ show todo | (tID, todo) <- reverse $ sortTodoList todoList]
                           where showTodoID = Printf.printf "%3d "

displayOnlyTodos :: TodoList -> String
displayOnlyTodos todoList = unlines [ show todo | (_, todo) <- reverse $ sortTodoList todoList]
