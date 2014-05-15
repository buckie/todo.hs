module Todo.Marshalling
( readTodoList
, serialiseTodoList
) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Control.Arrow (second)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Todo.Todo
import Todo.List

blankLine :: String -> Bool
blankLine = ([]==) . Text.unpack . Text.strip . Text.pack

-- FIXME: this should be the only place that knows about blank lines in
-- todo.txt

readTodoList :: String -> TodoList
readTodoList todoTxt = [(tID, readTodo todoLine) | (tID, todoLine) <- linesWithIDs, not $ blankLine todoLine]
                    where linesWithIDs = zip [(1::Int)..] (lines todoTxt)

readTodo :: String -> Todo
readTodo todoLine =
  case priorityChar' of
    Just pri -> prioritise (toUpper pri) $ unprioritise todo
    Nothing -> todo
  where todo = Todo todoLine
        priorityChar' = priorityChar todo

serialiseTodoList :: TodoList -> String
-- FIXME: this is where we "add in" blank lines for to fill in the gaps in
-- between [TodoID]
serialiseTodoList todoList =
  unlines $ map todoOrBlankLine lineIDs
  where
    serialisedTodoListWithIDs = map (Control.Arrow.second serialiseTodo) todoList
    todoMap = Map.fromList serialisedTodoListWithIDs

    todoIDs = map fst todoList
    lineIDs = [1..(maximum todoIDs)]

    todoOrBlankLine iD = fromMaybe "" (Map.lookup iD todoMap)

serialiseTodo :: Todo -> String
serialiseTodo (Todo text) = text
