{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Data.Maybe
import           Data.Profunctor
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

instance Editable Education where
  editor = do
    let selector x =
          case x of
            Basic_        -> Basic
            Intermediate_ -> Intermediate
            Other_ _      -> Other
    editorSum
      [ (Basic, const Basic_ <$> editorUnit)
      , (Intermediate, const Intermediate_ <$> editorUnit)
      , (Other, dimap (fromMaybe "" . getOther) Other_ editor)
      ]
      selector


data Person = Person
  { education           :: Education
  , firstName, lastName :: String
  , age                 :: Maybe Int
  , brexiteer           :: Bool
  , status              :: LegalStatus
  }
  deriving Show

instance Editable LegalStatus where
  editor = editorJust $ editorEnumBounded(pure(string.show))

instance Editable Person where
  editor =
    (\fn ln a e ls b -> Person e fn ln a b ls)
      <$> field "First:"     firstName editor
      -*- field "Last:"      lastName editor
      -*- field "Age:"       age editor
      -*- field "Education:" education editor
      -*- field "Status"     status editor
      -*- field "Brexiter"   brexiteer editor

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1 <- createEditor editor person1B
  person1B <- stepper (Person Basic_ "" "" Nothing False Single) (edited person1)

  getBody w #+ [grid
    [ [return $ editorElement person1]
    , [sink text (show <$> contents person1) p]
    ]]
