module List.List
  ( ID
  , Item(..)
  , List(..)
  , showBare
  , showBareList
  , fmapIf
  ) where

import qualified Data.Foldable as F

type ID = Int

data Item = Item ID a deriving (Eq)

newtype List a = List [Item a]

instance Functor Item where
  fmap f (Item idx x) = Item idx (f x)

fmapIf :: (Item a -> Bool) ->
          (a -> b) ->
          (a -> b) ->
          (Item a -> Item b)
fmapIf p f id' i = if p i
                   then fmap f i
                   else fmap id' i

instance (Show a) => Show (Item a) where
  show (Item idx x) = show idx ++ " " ++ show x
showBare :: (Show a) => Item a -> String
showBare (Item _ x) = show x

instance (Ord a) => Ord (Item a) where
  compare (Item id1 i1) (Item id2 i2) = case compare i1 i2 of
                                                  EQ -> compare id1 id2
                                                  x  -> x

instance Functor List where
  fmap f (List xs) = List $ fmap (fmap f) xs

instance (Show a) => Show (List a) where
  show (List xs) = unlines $ map show xs
showBareList :: (Show a) => List a -> String
showBareList (List xs) = unlines $ map showBare xs

-- FIXME: make this typecheck. I can't shake the feeling that i'm doing something wrong
instance F.Foldable List where
  -- foldr :: (a -> b -> b) -> b -> List a -> b
--  foldr f z (List is) = foldr (\i z' -> fromItem $ (fromItem . fmap f) i (Item 0 z')) z is
  foldr f z (List is) = foldr

fromItem :: Item a -> a
fromItem (Item _ x) = x
