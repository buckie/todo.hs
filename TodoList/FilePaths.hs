module TodoList.FilePaths
-- FIXME: TodoList is becoming a bit crowded... and FilePaths is more of
-- a todo.txt (interface) concern ...
-- maybe split into TodoTxt
( getTodoTxtFilePath
, getArchiveFilePath
) where

import System.Directory
import System.Environment (getEnv)

localTodoFilePath :: String
localTodoFilePath = "./todo.txt"

localArchiveFilePath :: String
localArchiveFilePath = "./archive.txt" -- FIXME: this should be done.txt

getTodoTxtFilePath :: IO FilePath
getTodoTxtFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
    then return localTodoFilePath
    else getEnv "TODO_FILE_PATH" -- FIXME: this should be TODO_TXT_FILE_PATH

getArchiveFilePath :: IO FilePath
getArchiveFilePath = do
  localTodoFile <- doesFileExist localTodoFilePath
  if localTodoFile
     then return localArchiveFilePath
     else getEnv "TODO_ARCHIVE_FILE_PATH" -- FIXME: this should be DONE_TXT

