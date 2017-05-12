{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Data.Profunctor
import qualified Generics.SOP                    as SOP
import           GHC.Generics
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
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance Editable LegalStatus
instance SOP.HasDatatypeInfo LegalStatus
instance SOP.Generic LegalStatus
instance Default LegalStatus where def = Single

data Education
  = Basic_
  | Intermediate_
  | Other_ String
  deriving (Eq, Ord, Read, Show, Generic)

instance Default Education where def = Basic_

getOther :: Education -> Maybe String
getOther (Other_ s) = Just s
getOther _          = Nothing

data EducationTag = Basic | Intermediate | Other deriving (Eq,Ord,Show, Generic)

editorEducation :: EditorFactory Education Education
editorEducation = do
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

instance Editable Education
instance SOP.HasDatatypeInfo Education
instance SOP.Generic Education

newtype Brexiteer = Brexiteer Bool deriving (Eq, Show, Ord, Generic)

instance Default Brexiteer where def = Brexiteer False
instance Editable Brexiteer where editor = editorGenericSimple
instance SOP.HasDatatypeInfo Brexiteer
instance SOP.Generic Brexiteer

data Person = Person
  { education           :: Education
  , firstName, lastName :: String
  , age                 :: Maybe Int
  , brexiteer           :: Brexiteer
  , status              :: LegalStatus
  }
  deriving (Generic, Show)

instance Editable Person
instance SOP.HasDatatypeInfo Person
instance SOP.Generic Person
instance Default Person where def = Person Basic_ "First" "Last" (Just 18) def def

editorLegalStatus :: EditorFactory LegalStatus LegalStatus
editorLegalStatus = editorJust $ editorEnumBounded(pure(string.show))

editorPerson :: EditorFactory Person Person
editorPerson =
    (\fn ln a e ls b -> Person e fn ln a b ls)
      <$> field "First:"     firstName editor
      -*- field "Last:"      lastName editor
      -*- field "Age:"       age editor
      -*- field "Education:" education editorEducation
      -*- field "Status"     status editorLegalStatus
      -*- field "Brexiter"   brexiteer editor

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1 <- createEditor editorPerson person1B
  person1B <- stepper def (edited person1)

  person2 <- createEditor editorGeneric person1B

  getBody w #+ [grid
    [ [return $ editorElement person1]
    , [hr]
    , [return $ editorElement person2]
    , [hr]
    , [sink text (show <$> contents person1) p]
    ]]
