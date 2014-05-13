import System.Environment (getArgs)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.Actions

todoFile :: FilePath
todoFile = "./t.txt"

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodoTxt $ readTodoTxt contents

add :: String -> IO ()
add todo = do
  appendFile todoFile $ todo ++ "\n"
  list

remove :: TodoId -> IO ()
remove = updateTodoTxtWith removeTodo

complete :: TodoId -> IO ()
complete = updateTodoTxtWith completeTodo

updateTodoFile :: [Todo] -> IO ()
updateTodoFile newTodoList = do
  let newContents = serialiseTodoTxt newTodoList
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFile
  renameFile tempName todoFile

type TodoUpdater = TodoId -> [Todo] -> (Maybe Todo, [Todo])
updateTodoTxtWith :: TodoUpdater -> TodoId -> IO ()
updateTodoTxtWith f targetTodoId = do
  contents <- readFile todoFile
  let todoTxt = readTodoTxt contents
  let (target, newTodoList) = f targetTodoId todoTxt
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
