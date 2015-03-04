module ToDo.Utils
  ( colouredStr
  , Color(..)
  ) where

import           System.Console.ANSI

colouredStr :: Color -> String -> String
colouredStr colour str =
  setColor colour ++ str ++ resetColor
  where
   setColor c = setSGRCode [SetColor Foreground Dull c]
   resetColor = setSGRCode []


