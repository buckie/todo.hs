module TodoList.FilePaths
( getTodoTxtFilePath
, getArchiveFilePath
) where

import System.Directory
import System.Environment (getEnv)

localTodoFilePath :: String
localTodoFilePath = "./todo.txt"

localArchiveFilePath :: String
localArchiveFilePath = "./archive.txt"

getTodoTxtFilePath :: IO FilePath
getTodoTxtFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
    then return localTodoFilePath
    else getEnv "TODO_FILE_PATH"

getArchiveFilePath :: IO FilePath
getArchiveFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
     then return localArchiveFilePath
     else getEnv "TODO_ARCHIVE_FILE_PATH"

