
module ToDo.Types where

data Todo = Todo
              { unCompleted :: Bool
              , unPriority  :: Char
              , unBody      :: String
              }
          | Empty
  deriving (Eq, Show)

type ID = Int
type Item = (ID, Todo)
type TodoList = [Item]

data Result = Updated Todo
            | Unchanged Todo
            | Removed Todo
            deriving (Eq, Show)

type ListResult = ([Result], TodoList)

fromItem :: Item -> Todo
fromItem = snd

itemId :: Item -> ID
itemId = fst

appendItem :: Todo -> [Item] -> [Item]
appendItem todo todoList = todoList ++ [(newIdx, todo)]
  where
    newIdx :: ID
    newIdx = 1 + (maximum $ itemId `fmap` todoList)

