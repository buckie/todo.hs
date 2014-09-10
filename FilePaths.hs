module FilePaths
( getTodoTxtFilePath
, getDoneTxtFilePath
) where

import System.Directory
import System.Environment (getEnv)

localTodoTxtPath :: String
localTodoTxtPath = "./todo.txt"

localDoneTxtPath :: String
localDoneTxtPath = "./done.txt"

-- FIXME: use a dotfile for these
-- TODO: create a dotfile on first run
getTodoTxtFilePath :: IO FilePath
getTodoTxtFilePath = do
  localTodoFile <- doesFileExist localTodoTxtPath
  if localTodoFile
    then return localTodoTxtPath
    else getEnv "TODO_TXT_PATH"

getDoneTxtFilePath :: IO FilePath
getDoneTxtFilePath = do
  localTodoFile <- doesFileExist localTodoTxtPath
  if localTodoFile
     then return localDoneTxtPath
     else getEnv "DONE_TXT_PATH"

