import System.Environment (getArgs)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.Actions

todoFile :: FilePath
todoFile = "./t.txt"

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodos $ readTodos contents

add :: String -> IO ()
add todo = do
  appendFile todoFile $ todo ++ "\n"
  list

remove :: TodoId -> IO ()
remove = updateTodoFileWith removeTodo

complete :: TodoId -> IO ()
complete = updateTodoFileWith completeTodo

updateTodoFile :: [Todo] -> IO ()
updateTodoFile newTodoList = do
  let newContents = serialiseTodos newTodoList
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFile
  renameFile tempName todoFile

updateTodoFileWith :: TodosUpdater -> TodoId -> IO ()
updateTodoFileWith f targetTodoId = do
  contents <- readFile todoFile
  let todos = readTodos contents
  let (target, newTodoList) = f targetTodoId todos
  case target of
    Just todo -> do
      updateTodoFile newTodoList
      list
      putStrLn $ "Updated: " ++ show todo
    Nothing -> putStrLn $ "Cannot find todo #" ++ show targetTodoId

dispatch :: [String] -> IO ()
dispatch [] = list
dispatch ("list":[]) = list
dispatch ("ls":[]) = list
dispatch ("add":todo) = add $ unwords todo
dispatch ("a":todo) = add $ unwords todo
dispatch ("remove":tId:[]) = remove (read tId :: TodoId)
dispatch ("rm":tId:[]) = remove (read tId :: TodoId)
dispatch ("complete":tId:[]) = complete (read tId :: TodoId)
dispatch ("do":tId:[]) = complete (read tId :: TodoId)
dispatch (invalidCommand:[]) = putStrLn $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn "Command not recognized"

main :: IO ()
main = do
  args <- getArgs
  dispatch args
