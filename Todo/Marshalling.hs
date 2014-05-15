module Todo.Marshalling
( readTodos
, serialiseTodos
) where

import Todo.Todo
import Data.Char (toUpper)

readTodos :: String -> [Todo]
readTodos todoTxt = map readTodo $ lines todoTxt

readTodo :: String -> Todo
readTodo text =
  case priorityChar' of
    Just pri -> prioritise (toUpper pri) $ unprioritise todo
    Nothing -> todo
  where todo = Todo text
        priorityChar' = priorityChar todo

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map serialiseTodo

serialiseTodo :: Todo -> String
serialiseTodo (Todo text) = text

