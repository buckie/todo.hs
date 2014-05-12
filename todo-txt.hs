import System.Environment (getArgs)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.TodoUtils

type TodoFile = FilePath
todoFile :: TodoFile
todoFile = "./t.txt"

dispatch :: [String] -> IO ()
dispatch [] = list
dispatch ("list":[]) = list
dispatch ("ls":[]) = list
dispatch ("add":todo) = add $ unwords todo
dispatch ("a":todo) = add $ unwords todo
dispatch ("remove":tId:[]) = remove (read tId :: Int)
dispatch ("rm":tId:[]) = remove (read tId :: Int)
dispatch ("complete":tId:[]) = complete (read tId :: Int)
dispatch ("do":tId:[]) = complete (read tId :: Int)
dispatch (invalidCommand:[]) = putStrLn $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn "Command not recognized"

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodoTxt $ readTodoTxt contents

add :: String -> IO ()
add todo = do
  appendFile todoFile $ todo ++ "\n"
  list

remove :: Int -> IO ()
remove = updateTodoTxtWith removeTodo

complete :: Int -> IO ()
complete = updateTodoTxtWith completeTodo

updateTodoTxtWith :: (Int -> [Todo] -> (Maybe Todo, [Todo])) -> Int -> IO ()
updateTodoTxtWith f targetTodoId = do

  contents <- readFile todoFile
  let todoTxt = readTodoTxt contents
  let (target, newTodoList) = f targetTodoId todoTxt

  case target of

    Just todo -> do
      let newContents = serialiseTodoTxt newTodoList
      (tempName, tempH) <- openTempFile "." "temp"
      hPutStr tempH newContents
      hClose tempH
      removeFile todoFile
      renameFile tempName todoFile

      list
      putStrLn $ "Updated: " ++ show todo

    Nothing -> putStrLn $ "Cannot find todo #" ++ show targetTodoId

main :: IO ()
main = do
  args <- getArgs
  dispatch args
