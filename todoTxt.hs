import System.Environment (getArgs)
import System.Process (runCommand)
import System.IO (openTempFile, hPutStr, hClose, hPutStr)
import System.Directory (removeFile, renameFile)

import Todo.Actions

todoFile :: FilePath
todoFile = "./t.txt"

updateTodoFile :: [Todo] -> IO ()
updateTodoFile newTodos = do
  let newContents = serialiseTodos newTodos
  (tempName, tempH) <- openTempFile "." "temp"
  hPutStr tempH newContents
  hClose tempH
  removeFile todoFile
  renameFile tempName todoFile

type UpdatedTodo = Todo
type TodosUpdater = [TodoID] -> [Todo] -> Maybe ([UpdatedTodo], [Todo])
updateTodoFileWith ::  TodosUpdater -> [TodoID] -> IO ()
updateTodoFileWith updateF targetTodoIDs = do
  contents <- readFile todoFile
  let oldTodos = readTodos contents
  let updateResult = updateF targetTodoIDs oldTodos
  case updateResult of
    Just (updatedTodos, newTodos) -> do
      updateTodoFile newTodos
      putStrLn $ "Todo(s) affected:\n" ++ unlines (map show updatedTodos)
    Nothing -> putStrLn $ "Could not find todo(s): " ++ show targetTodoIDs

list :: IO ()
list = do
  contents <- readFile todoFile
  putStrLn . displayTodos $ readTodos contents

add :: String -> IO ()
add todo = appendFile todoFile $ todo ++ "\n"

complete :: [TodoID] -> IO ()
complete = updateTodoFileWith completeTodos

uncomplete :: [TodoID] -> IO ()
uncomplete = updateTodoFileWith uncompleteTodos

prioritise :: Char -> [TodoID] -> IO ()
prioritise priorityChar = updateTodoFileWith (prioritiseTodos priorityChar)

unprioritise :: [TodoID] -> IO ()
unprioritise = updateTodoFileWith unprioritiseTodos

remove :: [TodoID] -> IO ()
remove targetTodoIDs = do
  putStrLn $ "Removing todos: " ++ show targetTodoIDs ++ "\n"
  updateTodoFileWith removeTodos targetTodoIDs

editTodoFile :: IO ()
editTodoFile = do
  _ <- runCommand $ "$EDITOR " ++ todoFile
  return ()

dispatch :: [String] -> IO ()
-- FIXME: getOpts or something a bit more solid/less ridiculous than this
dispatch [] = list
dispatch ("list":[]) = list
dispatch ("ls":[]) = list

dispatch ("add":todo) = add $ unwords todo
dispatch ("a":todo) = add $ unwords todo

dispatch ("complete":tIds) = complete $ map read tIds
dispatch ("do":tIds) = complete $ map read tIds

dispatch ("uncomplete":tIds) = uncomplete $ map read tIds
dispatch ("undo":tIds) = uncomplete $ map read tIds

dispatch ("prioritise":pri:tIds) = prioritise (head pri) $ map read tIds
dispatch ("pri":pri:tIds) = prioritise (head pri) $ map read tIds

dispatch ("unprioritise":tIds) = unprioritise $ map read tIds
dispatch ("unpri":tIds) = unprioritise $ map read tIds

dispatch ("remove":tIds) = remove $ map (\tId -> read tId :: Int) tIds
dispatch ("rm":tIds) = remove $ map read tIds

dispatch ("edit":[]) = editTodoFile
dispatch ("e":[]) = editTodoFile

dispatch (invalidCommand:[]) = putStrLn $ "Command not recognized: " ++ invalidCommand
dispatch _ = putStrLn "Command not recognized"

main :: IO ()
main = do
  args <- getArgs
  dispatch args
