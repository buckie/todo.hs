module TodoList.Marshalling
( readTodoList
, serialiseTodoList
) where

import Data.Maybe (fromMaybe)
import Control.Arrow (second)
import qualified Data.Text as Text
import qualified Data.Map as Map

import TodoList.Todo
import TodoList.List

blankLine :: String -> Bool
blankLine = ([]==) . Text.unpack . Text.strip . Text.pack

readTodoList :: String -> TodoList
readTodoList todoTxt = [(tID, readTodo todoLine) | (tID, todoLine) <- linesWithIDs, not $ blankLine todoLine]
                       where linesWithIDs = zip [(1::Int)..] (lines todoTxt)

readTodo :: String -> Todo
readTodo = normalise . Todo

serialiseTodoList :: TodoList -> String
serialiseTodoList [] = ""
serialiseTodoList todoList =
  unlines . removeConsecutiveBlankLines $ map todoOrBlankLine lineIDs
  where
    serialisedTodosWithIDs = map (Control.Arrow.second serialiseTodo) todoList

    serialisedTodos :: Map.Map TodoID String
    serialisedTodos = Map.fromList serialisedTodosWithIDs

    todoOrBlankLine iD = fromMaybe "" (Map.lookup iD serialisedTodos)

    todoIDs = map fst todoList
    lineIDs = [1..(maximum todoIDs)]

    removeConsecutiveBlankLines :: [String] -> [String]
    removeConsecutiveBlankLines [] = []
    removeConsecutiveBlankLines ("":"":strs) = "" : removeConsecutiveBlankLines strs
    removeConsecutiveBlankLines (str:strs) = str : removeConsecutiveBlankLines strs

serialiseTodo :: Todo -> String
serialiseTodo (Todo text) = text
