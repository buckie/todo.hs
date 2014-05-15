module Todo.List
( TodoID
, allTodosWithIDs
, todoList
, displayTodoList
, displayTodos
) where

import qualified Data.Text as Text
import qualified Text.Printf as Printf
import Data.List (sortBy)

import Todo.Todo

type TodoID = Int

allTodosWithIDs :: [Todo] -> [(TodoID, Todo)]
allTodosWithIDs = zip [(0::Int)..]

todoList :: [Todo] -> [(TodoID, Todo)]
todoList todos = filter (\(_, Todo tText) -> not $ blank tText) sortedTodosWithIDs
                 where sortedTodosWithIDs = sortBy (\(_, t1) (_, t2) -> compare t1 t2) $ allTodosWithIDs todos
                       blank = ([]==) . Text.unpack . Text.strip . Text.pack



-- Display the a todo list, with nicely formatted numbers
displayTodoList :: [Todo] -> String
displayTodoList todos = unlines [showTodoID tID ++ show todo | (tID, todo) <- todoList todos]
                        where showTodoID = Printf.printf "%3d "

-- Display the todos, no numbers
displayTodos :: [Todo] -> String
displayTodos todos = unlines [ show todo | (_, todo) <- todoList todos]
