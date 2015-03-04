module ToDo.FilePaths
( todoTxtFilePath
, doneTxtFilePath
) where

import System.Directory
import System.Environment (getEnv)

localTodoTxtPath :: String
localTodoTxtPath = "./todo.txt"

localDoneTxtPath :: String
localDoneTxtPath = "./done.txt"

todoTxtFilePath :: IO FilePath
todoTxtFilePath = do
  localTodoFile <- doesFileExist localTodoTxtPath
  -- FIXME: this should blow up if thre's no local todo.txt
  --        AND no environment var
  if localTodoFile
    then return localTodoTxtPath
    else getEnv "TODO_TXT_PATH"

doneTxtFilePath :: IO FilePath
doneTxtFilePath = do
  localTodoFile <- doesFileExist localTodoTxtPath
  -- FIXME: this should blow up if thre's no local todo.txt
  --        AND no environment var
  if localTodoFile
     then return localDoneTxtPath
     else getEnv "DONE_TXT_PATH"

