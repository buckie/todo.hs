import System.Environment (getArgs)
import System.Process (runCommand)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

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

type TodosUpdater = [TodoId] -> [Todo] -> Maybe [Todo]
-- FIXME: more meaningful "Todos affected" (Actually show the new todos)
-- FIXME: make sure that the "Todos affected" IDs reflect the ones
--        displayed by `list`
updateTodoFileWith ::  TodosUpdater -> [TodoId] -> IO ()
updateTodoFileWith updateF targetTodoIds = do
  contents <- readFile todoFile
  let oldTodos = readTodos contents
  let newTodos = updateF targetTodoIds oldTodos
  case newTodos of
    Just todos -> do
      list
      updateTodoFile todos
      putStrLn $ "Todo(s) affected: " ++ show targetTodoIds
    Nothing -> do
      list
      putStrLn $ "Could not find todo(s): " ++ show targetTodoIds

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodos $ readTodos contents

add :: String -> IO ()
add todo = do
  appendFile todoFile $ todo ++ "\n"
  list

complete :: [TodoId] -> IO ()
complete = updateTodoFileWith completeTodos

uncomplete :: [TodoId] -> IO ()
uncomplete = updateTodoFileWith uncompleteTodos

prioritise :: Char -> [TodoId] -> IO ()
prioritise priorityChar = updateTodoFileWith (prioritiseTodos priorityChar)

unprioritise :: [TodoId] -> IO ()
unprioritise = updateTodoFileWith unprioritiseTodos

remove :: [TodoId] -> IO ()
remove = updateTodoFileWith removeTodos

editTodoFile :: IO ()
editTodoFile = do
  _ <- runCommand $ "$EDITOR " ++ todoFile
  return ()

dispatch :: [String] -> IO ()
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
