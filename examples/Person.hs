{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Data.Maybe
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Editors
import           Graphics.UI.Threepenny.Elements

main :: IO ()
main = startGUI defaultConfig setup

data LegalStatus
  = Single
  | Married
  | Divorced
  | Widowed
  deriving (Bounded, Enum, Eq, Ord, Show)

data Education
  = Basic_
  | Intermediate_
  | Other_ String
  deriving (Eq, Ord, Read, Show)

getOther :: Education -> Maybe String
getOther (Other_ s) = Just s
getOther _          = Nothing

data EducationTag = Basic | Intermediate | Other deriving (Eq,Ord,Show)

data Person = Person
  { education           :: Education
  , firstName, lastName :: String
  , age                 :: Int
  , brexiteer           :: Bool
  , status              :: Maybe LegalStatus
  }
  deriving Show

instance Editable Education where
  editor b = do
    basicEditor <- fmap (const Basic_) <$> editor (pure ())
    intermediateEditor <- fmap (const Intermediate_) <$> editor (pure ())
    otherEditor <- fmap Other_ <$> editor (fromMaybe "" . getOther <$> b)
    let selector x =
          case x of
            Basic_        -> Basic
            Intermediate_ -> Intermediate
            Other_ _      -> Other
    editorSum
      [ (Basic, basicEditor)
      , (Intermediate, intermediateEditor)
      , (Other, otherEditor)
      ]
      selector
      b

instance Editable Person where
  editor b =
    fmap (\fn ln a e ls b -> Person e fn ln a b ls)
      <$> string "First:"     *| editor (firstName <$> b)
      -*- string "Last:"      *| editor (lastName <$> b)
      -*- string "Age:"       *| editor (age <$> b)
      -*- string "Education:" *| editor (education <$> b)
      -*- string "Status"     *| editorEnumBounded(pure(string.show)) (status <$> b)
      -*- string "Brexiter"   *| editor (brexiteer <$> b)

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1 :: Editor Person <- editor person1B
  person1B <- stepper (Person Basic_ "" "" 0 False Nothing) (edited person1)

  getBody w #+ [grid
    [ [return $ editorElement person1]
    , [sink text (show <$> contents person1) p]
    ]]
