import System.Environment (getArgs, getEnv)
import System.Process (runCommand)

import Control.Monad (when)
import Data.Char (toUpper)

import TodoList.Todo (Todo)
import TodoList.List (TodoID, TodoList, displayTodoList, displayTodos)
import TodoList.Actions
import TodoList.Marshalling
import TodoList.File

import TodoList.Utils

-- TODO: pick up a ./todo.txt file in the current dir, otherwise use
-- global
getTodoTxtFilePath :: IO FilePath
getTodoTxtFilePath = getEnv "TODO_TXT_PATH"

getArchiveFilePath :: IO FilePath
getArchiveFilePath = getEnv "TODO_ARCHIVE_PATH"

type UpdatedTodo = Todo
type TodosUpdater = [TodoID] -> TodoList -> Maybe (TodoList, TodoList)
-- FIXME: this looks like the bastard child of Todo.Actions and Todo.File
--        but it also has some interface stuff..
--        .. it does too much, it doesn't belong here
updateTodoFileWith ::  FilePath -> TodosUpdater -> [TodoID] -> IO ()
updateTodoFileWith todoFilePath updateF targetTodoIDs = do
  oldTodos <- readTodoFile todoFilePath
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodos, newTodos) -> do
      updateTodoFile todoFilePath newTodos
      putStrLn $ displayTodoList newTodos
      putStrLn $ "Todo(s) affected:\n" ++ displayTodos updatedTodos
    Nothing -> do
      putStrLn $ displayTodoList oldTodos
      putStrLn $ colouredStr Red $ "Could not find todo(s): " ++ show targetTodoIDs

list :: IO ()
list = do
  todoTxtFilePath <- getTodoTxtFilePath
  todos <- readTodoFile todoTxtFilePath
  putStrLn $ displayTodoList todos

add :: String -> IO ()
add todoText = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn "Adding todo:"
  let todos = readTodoList todoText
  putStrLn $ displayTodos todos
  appendTodoFile todoTxtFilePath todos

complete :: [TodoID] -> IO ()
complete targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn $ "Completing todo(s): " ++ show targetTodoIDs ++ "..."
  when (length targetTodoIDs > 3) (putStrLn "You are a machine!!")
  putStrLn ""
  updateTodoFileWith todoTxtFilePath completeTodos targetTodoIDs

uncomplete :: [TodoID] -> IO ()
uncomplete targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn $ "Hell yea! Reinstaing (un-completing) todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith todoTxtFilePath uncompleteTodos targetTodoIDs

prioritise :: Char -> [TodoID] -> IO ()
prioritise priorityChar targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn $ "Prioritizing todo(s) (priority " ++ [toUpper priorityChar] ++ "): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith todoTxtFilePath (prioritiseTodos priorityChar) targetTodoIDs

unprioritise :: [TodoID] -> IO ()
unprioritise targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn $ "Un-Prioritizing todo(s): " ++ show targetTodoIDs
  updateTodoFileWith todoTxtFilePath unprioritiseTodos targetTodoIDs

remove :: [TodoID] -> IO ()
remove targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn $ "Removing todo(s): " ++ show targetTodoIDs
  putStrLn $ "They weren't that important anyway" ++ "\n"
  updateTodoFileWith todoTxtFilePath removeTodos targetTodoIDs

archive :: IO ()
archive = do
  archiveFilePath <- getArchiveFilePath
  todoTxtFilePath <- getTodoTxtFilePath
  oldTodos <- readTodoFile todoTxtFilePath
  let archiveResult = archiveTodos oldTodos
  case archiveResult of
    Just (todosToArchive, updatedTodos) -> do
      putStrLn $ "Archiving todos (" ++ todoTxtFilePath ++ " -> " ++ archiveFilePath ++ " )...\n"
      appendTodoFile archiveFilePath todosToArchive
      updateTodoFile todoTxtFilePath updatedTodos
      putStrLn $ displayTodoList updatedTodos
      putStrLn $ show (length todosToArchive) ++ " todo(s) archived to " ++ archiveFilePath ++ ":\n"
      putStrLn $ displayTodos todosToArchive
    Nothing -> putStrLn $ colouredStr Red "Nothing to archive!"


editTodoFile :: IO ()
editTodoFile = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn "Editing todo.txt with $EDITOR"
  putStrLn "Mr. Sulu, you have the conn."
  _ <- runCommand $ "$EDITOR " ++ todoTxtFilePath
  return ()

dispatch :: [String] -> IO ()
-- FIXME: getOpts or something a bit more solid/less ridiculous than this
dispatch [] = list
dispatch ("list":[]) = list
dispatch ("ls":[]) = list

dispatch ("add":todo) = add $ unwords todo
dispatch ("a":todo) = add $ unwords todo

dispatch ("complete":tIds) = complete $ map read tIds
dispatch ("do":tIds) = complete $ map read tIds

dispatch ("uncomplete":tIds) = uncomplete $ map read tIds
dispatch ("undo":tIds) = uncomplete $ map read tIds

dispatch ("prioritise":pri:tIds) = prioritise (head pri) $ map read tIds
dispatch ("pri":pri:tIds) = prioritise (head pri) $ map read tIds

dispatch ("unprioritise":tIds) = unprioritise $ map read tIds
dispatch ("unpri":tIds) = unprioritise $ map read tIds

dispatch ("remove":tIds) = remove $ map (\tId -> read tId :: Int) tIds
dispatch ("rm":tIds) = remove $ map read tIds

dispatch ("archive":[]) = archive
dispatch ("ar":[]) = archive

dispatch ("edit":[]) = editTodoFile
dispatch ("e":[]) = editTodoFile

dispatch (invalidCommand:[]) = putStrLn $ colouredStr Red $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn $ colouredStr Red "Command not recognized"

main :: IO ()
main = do
  args <- getArgs
  dispatch args
