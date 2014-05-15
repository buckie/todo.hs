module TodoList.Utils
( colouredStr ) where

import System.Console.ANSI

type Colour = Color -- :P
colouredStr :: Colour -> String -> String
colouredStr colour str =
  setColor colour ++ str ++ resetColor
  where
   setColor c = setSGRCode [SetColor Foreground Dull c]
   resetColor = setSGRCode []


