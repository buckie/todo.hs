module FilePaths
( getTodoTxtFilePath
, getDoneTxtFilePath
) where

import System.Directory
import System.Environment (getEnv)

localTodoFilePath :: String
localTodoFilePath = "./todo.txt"

localArchiveFilePath :: String
localArchiveFilePath = "./done.txt"

getTodoTxtFilePath :: IO FilePath
getTodoTxtFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
    then return localTodoFilePath
    else getEnv "TODO_TXT_PATH"

getDoneTxtFilePath :: IO FilePath
getDoneTxtFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
     then return localArchiveFilePath
     else getEnv "DONE_TXT_PATH"

