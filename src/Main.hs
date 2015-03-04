module Main where

import           System.Environment (getArgs)
import           System.Process     (runCommand)

import           Control.Monad      (when)

import           Data.Char          (toUpper)

import           ToDo.ListActions

import           ToDo.TodoFile
import           ToDo.Marshalling
import           ToDo.Utils

import           ToDo.FilePaths

list :: IO ()
list = do
  todos <- readTodoFile =<< getTodoTxtFilePath
  putStrLn $ displayTodoList todos

add :: String -> IO ()
add todoText = do
  putStrLn "Adding todo(s):"
  let newTodos = readTodoList todoText
  putStrLn $ displayOnlyTodos newTodos
  todoTxtFilePath <- getTodoTxtFilePath
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

prepend :: String -> TargetTodoIDs -> IO ()
prepend textToPrepend targetTodoIDs = do
  putStrLn $ "Prepending \"" ++ textToPrepend ++ "\" to todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith (prependToTodos textToPrepend) targetTodoIDs

append :: String -> TargetTodoIDs -> IO ()
append textToAppend targetTodoIDs = do
  putStrLn $ "Appending \"" ++ textToAppend ++ "\" to todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith (appendToTodos textToAppend) targetTodoIDs

remove :: TargetTodoIDs -> IO ()
remove targetTodoIDs = do
  putStrLn $ "Removing todo(s): " ++ show targetTodoIDs
  putStrLn $ "They weren't that important anyway" ++ "\n"
  updateTodoFileWith removeTodos targetTodoIDs

archive :: IO ()
archive = do
  -- FIXME: archive can probably be a combo of append + remove
  archiveFilePath <- getDoneTxtFilePath
  todoTxtFilePath <- getTodoTxtFilePath

  oldTodos <- readTodoFile todoTxtFilePath
  let archiveResult = archiveTodoList oldTodos

  case archiveResult of

    Just (archivedTodoList, updatedTodoList) -> do
      putStrLn $ "Archiving todos (" ++ todoTxtFilePath ++ " -> " ++ archiveFilePath ++ " )...\n"
      appendTodoFile archiveFilePath archivedTodoList
      updateTodoFile todoTxtFilePath updatedTodoList
      putStrLn $ displayTodoList updatedTodoList
      putStrLn $ show (length archivedTodoList) ++ " todo(s) archived to " ++ archiveFilePath ++ ":\n"
      putStrLn $ displayOnlyTodos archivedTodoList

    Nothing -> putStrLn $ colouredStr Red "Nothing to archive!"

-- FIXME: editTodoFile doesn't work for some reason.
editTodoFile :: IO ()
editTodoFile = do
  todoTxtFilePath <- getTodoTxtFilePath
  putStrLn "Editing todo.txt with $EDITOR"
  putStrLn "Mr. Sulu, you have the conn."
  _ <- runCommand $ "$EDITOR " ++ todoTxtFilePath
  return ()

dispatch :: [String] -> IO ()
-- FIXME: getOpts or something a bit more solid/less ridiculous than dispatch fn
-- TODO: add CLI help!
-- TODO: add -g override flag that forces using global todo.txt file path
dispatch [] = list
dispatch ["list"] = list
dispatch ["ls"] = list

dispatch ("add":todo) = add $ unwords todo
dispatch ("a":todo) = add $ unwords todo

dispatch ("complete":tIDs) = complete $ map read tIDs
dispatch ("do":tIDs) = complete $ map read tIDs

dispatch ("uncomplete":tIDs) = uncomplete $ map read tIDs
dispatch ("undo":tIDs) = uncomplete $ map read tIDs

dispatch ("prioritise":pri:tIDs) = prioritise (toUpper $ head pri) $ map read tIDs
dispatch ("pri":pri:tIDs) = prioritise (toUpper $ head pri) $ map read tIDs

dispatch ("unprioritise":tIDs) = unprioritise $ map read tIDs
dispatch ("unpri":tIDs) = unprioritise $ map read tIDs

dispatch ("prepend":textToPrepend:tIDs) = prepend textToPrepend $ map read tIDs
dispatch ("prep":textToPrepend:tIDs) = prepend textToPrepend $ map read tIDs

dispatch ("append":textToAppend:tIDs) = append textToAppend $ map read tIDs
dispatch ("app":textToAppend:tIDs) = append textToAppend $ map read tIDs

dispatch ("remove":tIDs) = remove $ map (\tID -> read tID :: Int) tIDs
dispatch ("rm":tIDs) = remove $ map read tIDs

dispatch ["archive"] = archive
dispatch ["arr"] = do
  putStrLn $ colouredStr Yellow "☠ ☠ [Pirate Mode Enabled] ☠ ☠"
  putStrLn $ oldSalt ++ "Aye aye sir."
  putStrLn $ oldSalt ++ "You heard the Captain! Batten down the hatches, ye miserable scallywags"
  archive
  where oldSalt = colouredStr Yellow "old-salt: "
dispatch ["ar"] = archive

dispatch ["edit"] = editTodoFile
dispatch ["e"] = editTodoFile

dispatch [invalidCommand] = putStrLn $ colouredStr Red $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn $ colouredStr Red "Command not recognized"

main :: IO ()
main = do
  args <- getArgs
  dispatch args



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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
      putStrLn $ colouredStr Red $ "Could not find todo(s): " ++ show targetTodoID
