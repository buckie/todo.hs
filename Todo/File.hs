module Todo.File
( readTodoFile
, updateTodoFile
, updateTodoFileWith
, appendTodoFile
) where

import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.Todo
import Todo.Actions

readTodoFile :: FilePath -> IO [Todo]
readTodoFile todoFilePath = do
  contents <- readFile todoFilePath
  return $ readTodos contents

type UpdatedTodo = Todo
type TodosUpdater = [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])

updateTodoFile :: FilePath -> [Todo] -> IO ()
updateTodoFile todoFilePath newTodos = do
  let newContents = serialiseTodos newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFilePath
  renameFile tempName todoFilePath

updateTodoFileWith ::  FilePath -> TodosUpdater -> [TodoID] -> IO ()
updateTodoFileWith todoFilePath updateF targetTodoIDs = do
  oldTodos <- readTodoFile todoFilePath
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodos, newTodos) -> do
      updateTodoFile todoFilePath newTodos
      putStrLn $ displayTodoList newTodos
      putStrLn $ "Todo(s) affected:\n" ++ displayTodos updatedTodos
    Nothing -> do
      putStrLn $ displayTodoList oldTodos
      putStrLn $ "Could not find todo(s): " ++ show targetTodoIDs

appendTodoFile :: FilePath -> [Todo] -> IO ()
appendTodoFile todoFilePath todos = appendFile todoFilePath $ serialiseTodos todos

