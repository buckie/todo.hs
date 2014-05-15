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
type TodoList = [(TodoID, Todo)]

sortTodoList :: TodoList -> TodoList
sortTodoList = sortBy (\(_, t1) (_, t2) -> compare t1 t2)

displayTodoList :: TodoList -> String
displayTodoList todoList = unlines [showTodoID tID ++ show todo | (tID, todo) <- sortTodoList todoList]
                           where showTodoID = Printf.printf "%3d "

displayOnlyTodos :: TodoList -> String
displayOnlyTodos todoList = unlines [ show todo | (_, todo) <- sortTodoList todoList]
