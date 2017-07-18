{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Data.Coerce
import           Data.Default
import           Data.Maybe
import           Data.Profunctor
import qualified Generics.SOP                    as SOP
import           GHC.Generics
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Editors hiding (Single)
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
  = Basic
  | Intermediate
  | Other String
  deriving (Eq, Ord, Read, Show, Generic)

instance Default Education where def = Basic

getOther :: Education -> Maybe String
getOther (Other s) = Just s
getOther _         = Nothing

editorEducation :: EditorFactory Education Education
editorEducation = do
    let selector x = case x of
            Other _ -> "Other"
            _       -> show x
    editorSum horizontal
      [ ("Basic", const Basic <$> editorUnit)
      , ("Intermediate", const Intermediate <$> editorUnit)
      , ("Other", dimap (fromMaybe "" . getOther) Other editor)
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
instance Default Person where def = Person Basic "First" "Last" (Just 18) def def

editorPersonRows :: EditorFactory Person Person
editorPersonRows = vertically $ do
  (firstName, lastName) <- coerce $ horizontally $ do
      firstName <- coerce $ field "First:"     firstName editor
      lastName  <- coerce $ field "Last:"      lastName editor
      return (firstName, lastName)
  (age, education) <- coerce $ horizontally $ do
      age       <- coerce $ field "Age:"       age editor
      education <- coerce $ field "Education:" education editorEducation
      return (age, education)
  (status, brexiteer) <- coerce $ horizontally $ do
      status    <- coerce $ field "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- coerce $ field "Brexiter"   brexiteer editor
      return (status, brexiteer)
  return Person{..}

editorPersonColumns :: EditorFactory Person Person
editorPersonColumns = horizontally $ do
  (firstName, lastName, age) <- coerce $ vertically $ do
      firstName <- coerce $ field "First:"     firstName editor
      lastName  <- coerce $ field "Last:"      lastName editor
      age       <- coerce $ field "Age:"       age editor
      return (firstName, lastName, age)
  (education, status, brexiteer) <- coerce $ vertically $ do
      education <- coerce $ field "Education:" education editorEducation
      status    <- coerce $ field "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- coerce $ field "Brexiter"   brexiteer editor
      return (education, status, brexiteer)
  return Person{..}

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1r <- createEditor editorPersonRows person1B
  person1c <- createEditor editorPersonColumns person1B
  person2 <- createEditor editorGeneric person1B
  person1B <- stepper def (head <$> unions [edited person1r, edited person1c, edited person2])

  getBody w #+ [grid
    [ [return $ editorElement person1r]
    , [hr]
    , [return $ editorElement person1c]
    , [hr]
    , [return $ editorElement person2]
    , [hr]
    ]]
