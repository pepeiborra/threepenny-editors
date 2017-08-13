module Graphics.UI.Threepenny.Editors.Validation
  ( ValidationResult(..)
  , Validable(..)
  , isValid
  , updateIfValid
  ) where

data ValidationResult = Ok | Invalid String
instance Show ValidationResult where
  show Ok            = ""
  show (Invalid msg) = msg

class Validable a where
  validate :: a -> ValidationResult

isValid :: Validable a => a -> Bool
isValid x
  | Ok <- validate x = True
  | otherwise = False

updateIfValid :: Validable a => a -> a -> a
updateIfValid new old
  | isValid new = new
  | otherwise   = old
