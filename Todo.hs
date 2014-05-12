module Todo
( readTodoTxt
, serialiseTodoTxt
, displayTodoTxt
, removeTodo
, completeTodo
) where

import Data.List (partition)
import qualified Data.Text as Text

data Todo = Todo { todoId :: Int
                 , fullText :: String
                 , completed :: Bool
                 } deriving (Eq)

instance Show Todo where
    show (Todo tId tFullText _) = show tId ++ " " ++ tFullText

makeTodo :: Int -> String -> Todo
makeTodo tId tFullText = Todo { todoId = tId
                              , fullText = tFullText
                              , completed = isCompleted tFullText}
                           where isCompleted ('x':' ':_) = True
                                 isCompleted _ = False

readTodoTxt :: String -> [Todo]
readTodoTxt todoTxt = zipWith makeTodo [(0::Int)..] $ filter (not . blank ) $ lines todoTxt
              where blank :: String -> Bool
                    blank = ([]==) . Text.unpack . Text.strip . Text.pack

displayTodoTxt :: [Todo] -> String
displayTodoTxt = unlines . map show

serialiseTodoTxt :: [Todo] -> String
serialiseTodoTxt = unlines . map fullText

removeTodo :: Int -> [Todo] -> (Maybe Todo, [Todo])
removeTodo targetTodoId todoList =
  (removedTodo, newTodoList)
  where (removedTodos, newTodoList) = partition (\todo -> targetTodoId == todoId todo) todoList
        removedTodo = case removedTodos of
                          [] -> Nothing
                          (t:[]) -> Just t
                          _ -> error $ "No way! Found more than one todo with id #" ++ show targetTodoId

completeTodo :: Int -> [Todo] -> (Maybe Todo, [Todo])
completeTodo targetTodoId todoList =
    (completedTodo, newTodoList)
    where (foundTodo, todoListWithoutFoundTodo) = removeTodo targetTodoId todoList
          (completedTodo, newTodoList) = case foundTodo of
                                           Just t -> (Just (complete t), todoListWithoutFoundTodo ++ [complete t])
                                           Nothing -> (Nothing, todoList)

                                         where complete t = Todo { todoId = todoId t
                                                                 , fullText = "x " ++ fullText t
                                                                 , completed = True
                                                                 }



