-- TODO: Make a typeclass: Serialiseable
-- TODO: instance (Serialiseable a) => Serialiseable [a]
-- TODO: instance Serialiseable fucking Item
-- TODO: fucking instance Serialiseable fucking Todo
-- TODO: instance fucking Serialiseable fucking Result
-- get rid of this
module Marshalling
( readTodoList
, serialiseTodoList
) where

import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

import           List.List
import           Todo

blankLine :: String -> Bool
blankLine = ([]==)

readTodoList :: String -> TodoList
readTodoList todoTxt = TodoList [ListItem tID (readTodo todoLine) | (tID, todoLine) <- linesWithIDs
                                                                  , not $ blankLine todoLine]
                       where linesWithIDs = zip [(1::Int)..] (lines todoTxt)

readTodo :: String -> Todo
readTodo = Todo

-- FIXME: this seems an abomination
-- isn't there a TypeClass that does this?
serialiseTodoList :: TodoList -> String
serialiseTodoList (TodoList []) = ""
serialiseTodoList (TodoList items) =
  unlines $ map todoOrBlankLine lineNumbers
  where
    todoIDs = map (\(ListItem idx _) -> idx) items
    lineNumbers = [1..(maximum todoIDs)]

    serialisedTodos = Map.fromList $ map (\(ListItem idx t)-> (idx, serialiseTodo t)) items
    todoOrBlankLine idx = fromMaybe "" (Map.lookup idx serialisedTodos)

serialiseTodo :: Todo -> String
serialiseTodo (Todo t) = t
