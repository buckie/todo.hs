module FilePaths
( getTodoTxtFilePath
, getDoneTxtFilePath
) where

import System.Directory
import System.Environment (getEnv)

-- FIXME: should be localTodoTxtPath
localTodoFilePath :: String
localTodoFilePath = "./todo.txt"

-- FIXME: should be localDoneTxtPath
localArchiveFilePath :: String
localArchiveFilePath = "./done.txt"

-- FIXME: use a dotfile for these
-- TODO: create a dotfile on first run
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

