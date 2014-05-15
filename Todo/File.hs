module Todo.File
( readTodoFile
, updateTodoFile
, appendTodoFile
) where

import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.Todo
import Todo.Marshalling

readTodoFile :: FilePath -> IO [Todo]
readTodoFile todoFilePath = do
  contents <- readFile todoFilePath
  return $ readTodos contents

updateTodoFile :: FilePath -> [Todo] -> IO ()
updateTodoFile todoFilePath newTodos = do
  let newContents = serialiseTodos newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFilePath
  renameFile tempName todoFilePath

appendTodoFile :: FilePath -> [Todo] -> IO ()
appendTodoFile todoFilePath todos = appendFile todoFilePath $ serialiseTodos todos
