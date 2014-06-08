module TodoList.List
( TodoID
, TodoList
, displayTodoList
, displayOnlyTodos
) where

import qualified Text.Printf as Printf
import Data.List (sortBy, partition)

import TodoList.Todo

type ID = Int
data ListItem i = ListItem ID i deriving (Eq)

instance Functor ListItem where
  fmap f (ListItem idx item) = ListItem idx (f item)

instance (Show i) => Show (ListItem i) where
  show (ListItem tID todo) = showTodoID tID ++ show todo
                             where showTodoID = Printf.printf "%3d "

instance (Ord i) => Ord (ListItem i) where
  compare (ListItem _ i1) (ListItem _ i2) = compare i1 i2

type TodoList = [ListItem Todo]
type TodoID = ID

displayTodoList :: TodoList -> String
displayTodoList = unlines . map show . sortBy (flip compare)

displayOnlyTodos :: TodoList -> String
displayOnlyTodos = unlines . map showTodo . sortBy (flip compare)
                   where showTodo (ListItem _ t) = show t

partitionTodoList :: (Todo -> Bool) -> TodoList -> (TodoList, TodoList)
partitionTodoList p todoList = partition 

