module ToDo.TodoFile
( readTodoFile
, updateTodoFile
, appendTodoFile
) where

import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import ToDo.Marshalling
import ToDo.Types

readTodoFile :: FilePath -> IO TodoList
readTodoFile todoFilePath = do
  contents <- readFile todoFilePath
  return $ readTodoList contents

updateTodoFile :: FilePath -> TodoList -> IO ()
updateTodoFile todoFilePath newTodos = do
  let newContents = serialiseTodoList newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFilePath
  renameFile tempName todoFilePath

appendTodoFile :: FilePath -> TodoList -> IO ()
appendTodoFile todoFilePath todos = appendFile todoFilePath $ serialiseTodoList todos
