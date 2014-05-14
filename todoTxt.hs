import System.Environment (getArgs)
import System.Process (runCommand)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Control.Monad (when)
import Data.Char (toUpper)

import Todo.Actions

todoFile :: FilePath
todoFile = "./t.txt"

updateTodoFile :: [Todo] -> IO ()
updateTodoFile newTodos = do
  let newContents = serialiseTodos newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFile
  renameFile tempName todoFile

type UpdatedTodo = Todo
type TodosUpdater = [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
updateTodoFileWith ::  TodosUpdater -> [TodoID] -> IO ()
updateTodoFileWith updateF targetTodoIDs = do
  contents <- readFile todoFile
  let oldTodos = readTodos contents
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodos, newTodos) -> do
      updateTodoFile newTodos
      putStrLn $ displayTodos newTodos
      putStrLn $ "Todo(s) affected:\n" ++ unlines (map show updatedTodos)
    Nothing -> do
      putStrLn $ displayTodos oldTodos
      putStrLn $ "Could not find todo(s): " ++ show targetTodoIDs

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodos $ readTodos contents

add :: String -> IO ()
add todo = do
  putStrLn "Adding todo..."
  print todo
  putStrLn "\n"
  appendFile todoFile $ todo ++ "\n"

complete :: [TodoID] -> IO ()
complete targetTodoIDs = do
  putStrLn $ "Completing todo(s): " ++ show targetTodoIDs ++ "..."
  when (length targetTodoIDs > 3) (putStrLn "You are a machine!!")
  putStrLn ""
  updateTodoFileWith completeTodos targetTodoIDs

uncomplete :: [TodoID] -> IO ()
uncomplete targetTodoIDs = do
  putStrLn $ "Hell yea! Reinstaing (un-completing) todo(s): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith uncompleteTodos targetTodoIDs

prioritise :: Char -> [TodoID] -> IO ()
prioritise priorityChar targetTodoIDs = do
  putStrLn $ "Prioritizing todo(s) (priority " ++ [toUpper priorityChar] ++ "): " ++ show targetTodoIDs ++ "...\n"
  updateTodoFileWith (prioritiseTodos priorityChar) targetTodoIDs

unprioritise :: [TodoID] -> IO ()
unprioritise targetTodoIDs = do
  putStrLn $ "Un-Prioritizing todo(s): " ++ show targetTodoIDs
  updateTodoFileWith unprioritiseTodos targetTodoIDs

remove :: [TodoID] -> IO ()
remove targetTodoIDs = do
  putStrLn $ "Removing todo(s): " ++ show targetTodoIDs
  putStrLn $ "They weren't that important anyway" ++ "\n"
  updateTodoFileWith removeTodos targetTodoIDs

editTodoFile :: IO ()
editTodoFile = do
  putStrLn "Editing todo(s) with $EDITOR"
  putStrLn "Mr. Sulu, you have the conn."
  _ <- runCommand $ "$EDITOR " ++ todoFile
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
