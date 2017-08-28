{- | Simple helpers to perform validation of user input.

   @
     -- update only if valid
     value <- accumB def updateIfValid userEdits

     -- collect validation warnings
     warnings <- stepper ok validate userEdits
   @
-}

module Graphics.UI.Threepenny.Editors.Validation
  ( ValidationResult
  , ok
  , fromWarnings
  , getWarnings
  , Validable(..)
  , isValid
  , updateIfValid
  ) where

newtype ValidationResult = ValidationResult [String]

-- | All is good
ok :: ValidationResult
ok = ValidationResult []

-- | Create a validation result from a list of warnings.
--
--  > fromWarnings [] = ok
fromWarnings :: [String] -> ValidationResult
fromWarnings = ValidationResult

getWarnings :: ValidationResult -> [String]
getWarnings (ValidationResult ww) = ww

instance Show ValidationResult where
  show = unlines . getWarnings

-- | The class of values that support validation.
class Validable a where
  validate :: a -> ValidationResult

isValid :: Validable a => a -> Bool
isValid x
  | ValidationResult [] <- validate x = True
  | otherwise = False

updateIfValid :: Validable a => a -> a -> a
updateIfValid new old
  | isValid new = new
  | otherwise   = old
