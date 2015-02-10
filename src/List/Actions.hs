module List.Actions
( Result(..)
, ListResult
, updateItems
, removeItems
, archiveItems
, fromResult
) where

import           List.List

data Result a = Updated a
              | Unchanged a
              | Removed a

type ListResult a = ([Result a], List a)

updateItems :: (Item a -> Item (Result a)) -> List a -> ListResult a
updateItems f xs = (onlyUpdated, newList)
                   where
                     results = map f xs
                     onlyUpdated = filter (\(Item _ r) -> isChanged r) results
                     newList = foldl (\is ir -> if isTrue $ fmap isRemoved ir
                                                then xs
                                                else fmap fromResult ir:is) [] results

removeItems :: [ID] -> List a -> ListResult a
removeItems targetIDs = updateItems remove
                        where remove = fmapIf (isPresent targetIDs) Removed Unchanged

archiveItems :: (a -> Bool) -> List a -> ListResult a
archiveItems p = updateItems archive
                 where
                   archive = fmapIf' p Removed Unchanged

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
fromResult :: Result a -> a
fromResult (Updated x) = x
fromResult (Unchanged x) = x
fromResult (Removed x) = x

isPresent :: [ID] -> Item a -> Bool
isPresent targetIDs (Item i _) = i `elem` targetIDs

instance Functor Result where
  fmap f (Updated x) = Updated $ f x
  fmap f (Unchanged x) = Unchanged $ f x
  fmap f (Removed x) = Removed $ f x

isChanged :: Result a -> Bool
isChanged (Unchanged _) = False
isChanged _ = True

isRemoved :: Result a -> Bool
isRemoved (Removed _) = True
isRemoved _ = False

isTrue :: Item Bool -> Bool
isTrue (Item _ x) = x

fmapIf' :: (a -> Bool) ->
           (a -> b) ->
           (a -> b) ->
           (Item a -> Item b)
fmapIf' p = fmapIf (\(Item _ x) -> p x)
