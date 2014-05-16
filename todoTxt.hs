import System.Environment (getArgs, getEnv)
import System.Process (runCommand)
import System.Directory (doesFileExist)

import Control.Monad (when)
import Data.Char (toUpper)

import TodoList.List (displayTodoList, displayOnlyTodos)
import TodoList.Actions
import TodoList.Marshalling
import TodoList.File
import TodoList.FilePaths

import TodoList.Utils

list :: IO ()
list = do
  todoTxtFilePath <- getTodoTxtFilePath
  todos <- readTodoFile todoTxtFilePath
  putStrLn $ displayTodoList todos

add :: String -> IO ()
add todoText = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn "Adding todo:"
  let newTodos = readTodoList todoText
  putStrLn $ displayOnlyTodos newTodos
  appendTodoFile todoTxtFilePath newTodos

complete :: TargetTodoIDs -> IO ()
complete targetTodoIDs = do
  putStrLn $ "Completing todo(s): " ++ show targetTodoIDs ++ "..."
  when (length targetTodoIDs > 3) (putStrLn "You are a machine!!")
  putStrLn ""
  updateTodoFileWith completeTodos targetTodoIDs

uncomplete :: TargetTodoIDs -> IO ()
uncomplete targetTodoIDs = do
  putStrLn $ "Hell yea! Officially reinstaing (un-completing) todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith uncompleteTodos targetTodoIDs

prioritise :: Char -> TargetTodoIDs -> IO ()
prioritise priorityChar targetTodoIDs = do
  putStrLn $ "Prioritizing todo(s) with (" ++ [toUpper priorityChar] ++ "): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith (prioritiseTodos priorityChar) targetTodoIDs

unprioritise :: TargetTodoIDs -> IO ()
unprioritise targetTodoIDs = do
  putStrLn $ "Un-Prioritizing todo(s): " ++ show targetTodoIDs
  updateTodoFileWith unprioritiseTodos targetTodoIDs

remove :: TargetTodoIDs -> IO ()
remove targetTodoIDs = do
  putStrLn $ "Removing todo(s): " ++ show targetTodoIDs
  putStrLn $ "They weren't that important anyway" ++ "\n"
  updateTodoFileWith removeTodos targetTodoIDs

archive :: IO ()
archive = do
  archiveFilePath <- getArchiveFilePath
  todoTxtFilePath <- getTodoTxtFilePath
  oldTodos <- readTodoFile todoTxtFilePath
  let archiveResult = archiveTodos oldTodos
  case archiveResult of
    Just (archivedTodoList, updatedTodoList) -> do
      putStrLn $ "Archiving todos (" ++ todoTxtFilePath ++ " -> " ++ archiveFilePath ++ " )...\n"
      appendTodoFile archiveFilePath archivedTodoList
      updateTodoFile todoTxtFilePath updatedTodoList
      putStrLn $ displayTodoList updatedTodoList
      putStrLn $ show (length archivedTodoList) ++ " todo(s) archived to " ++ archiveFilePath ++ ":\n"
      putStrLn $ displayOnlyTodos archivedTodoList
    Nothing -> putStrLn $ colouredStr Red "Nothing to archive!"

updateTodoFileWith :: TodoListUpdateAction -> TargetTodoIDs -> IO ()
updateTodoFileWith updateF targetTodoIDs = do
  todoTxtFilePath <- getTodoTxtFilePath
  oldTodos <- readTodoFile todoTxtFilePath
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodoList, newTodoList) -> do
      updateTodoFile todoTxtFilePath newTodoList
      putStrLn $ displayTodoList newTodoList
      putStrLn $ "Todo(s) affected:\n" ++ displayOnlyTodos updatedTodoList
    Nothing -> do
      putStrLn $ displayTodoList oldTodos
      putStrLn $ colouredStr Red $ "Could not find todo(s): " ++ show targetTodoIDs

editTodoFile :: IO ()
editTodoFile = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn "Editing todo.txt with $EDITOR"
  putStrLn "Mr. Sulu, you have the conn."
  _ <- runCommand $ "$EDITOR " ++ todoTxtFilePath
  return ()

-- FIXME: getOpts or something a bit more solid/less ridiculous than this
-- TODO: add CLI help!
dispatch :: [String] -> IO ()
-- TODO: add filtered ls for contexts and projects
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
