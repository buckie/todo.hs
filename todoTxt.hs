import System.Environment (getArgs)
import System.Process (runCommand)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Control.Monad (when)
import Data.Char (toUpper)

import Todo.Actions

todoTxtFilePath :: FilePath
todoTxtFilePath = "./t.txt"

archiveFilePath :: FilePath
archiveFilePath = "./archive.txt"

-- ################################################################
-- FIXME: Move this to some Todo.File module

readTodoFile :: FilePath -> IO [Todo]
readTodoFile todoFilePath = do
  contents <- readFile todoFilePath
  return $ readTodos contents

type UpdatedTodo = Todo
type TodosUpdater = [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])

updateTodoFile :: FilePath -> [Todo] -> IO ()
updateTodoFile todoFilePath newTodos = do
  let newContents = serialiseTodos newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFilePath
  renameFile tempName todoFilePath

updateTodoFileWith ::  FilePath -> TodosUpdater -> [TodoID] -> IO ()
updateTodoFileWith todoFilePath updateF targetTodoIDs = do
  oldTodos <- readTodoFile todoFilePath
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodos, newTodos) -> do
      updateTodoFile todoFilePath newTodos
      putStrLn $ displayTodoList newTodos
      -- FIXME: should have a method to display todos
      putStrLn $ "Todo(s) affected:\n" ++ unlines (map show updatedTodos)
    Nothing -> do
      putStrLn $ displayTodoList oldTodos
      putStrLn $ "Could not find todo(s): " ++ show targetTodoIDs

appendTodoFile :: FilePath -> [Todo] -> IO ()
appendTodoFile todoFilePath todos = appendFile todoFilePath $ serialiseTodos todos

-- ################################################################

list :: IO ()
list = do
  todos <- readTodoFile todoTxtFilePath
  putStrLn $ displayTodoList todos

add :: String -> IO ()
add todoText = do
  putStrLn "Adding todo:"
  let todos = readTodos todoText
  putStrLn $ displayTodos todos
  appendTodoFile todoTxtFilePath todos

complete :: [TodoID] -> IO ()
complete targetTodoIDs = do
  putStrLn $ "Completing todo(s): " ++ show targetTodoIDs ++ "..."
  when (length targetTodoIDs > 3) (putStrLn "You are a machine!!")
  putStrLn ""
  updateTodoFileWith todoTxtFilePath completeTodos targetTodoIDs

uncomplete :: [TodoID] -> IO ()
uncomplete targetTodoIDs = do
  putStrLn $ "Hell yea! Reinstaing (un-completing) todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith todoTxtFilePath uncompleteTodos targetTodoIDs

prioritise :: Char -> [TodoID] -> IO ()
prioritise priorityChar targetTodoIDs = do
  putStrLn $ "Prioritizing todo(s) (priority " ++ [toUpper priorityChar] ++ "): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith todoTxtFilePath (prioritiseTodos priorityChar) targetTodoIDs

unprioritise :: [TodoID] -> IO ()
unprioritise targetTodoIDs = do
  putStrLn $ "Un-Prioritizing todo(s): " ++ show targetTodoIDs
  updateTodoFileWith todoTxtFilePath unprioritiseTodos targetTodoIDs


remove :: [TodoID] -> IO ()
remove targetTodoIDs = do
  putStrLn $ "Removing todo(s): " ++ show targetTodoIDs
  putStrLn $ "They weren't that important anyway" ++ "\n"
  updateTodoFileWith todoTxtFilePath removeTodos targetTodoIDs

editTodoFile :: IO ()
editTodoFile = do
  putStrLn "Editing todo(s) with $EDITOR"
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

dispatch ("edit":[]) = editTodoFile
dispatch ("e":[]) = editTodoFile

dispatch (invalidCommand:[]) = putStrLn $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn "Command not recognized"

main :: IO ()
main = do
  args <- getArgs
  dispatch args
