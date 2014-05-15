module Todo.Marshalling
( readTodos
, serialiseTodos
) where

import Todo.Todo

readTodos :: String -> [Todo]
-- FIXME: maybe this is not the right place .. but priority needs to be
-- uppercased; even if I the input to this fn is lowercase
readTodos todoTxt = map Todo $ lines todoTxt

serialiseTodos :: [Todo] -> String
serialiseTodos = unlines . map (\(Todo text) -> text)

