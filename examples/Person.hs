{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
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
import           Graphics.UI.Threepenny.Editors.Types
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

editorEducation :: EditorFactory Layout Education Education
editorEducation = do
    let selector x = case x of
            Other _ -> "Other"
            _       -> show x
    editorSum beside
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

editorPersonHorizontal, editorPersonVertical :: EditorFactory Layout Person Person
editorPersonHorizontal = construct $ do
  (firstName, lastName) <- withLayout Vertical $ construct $ do
      firstName <- fieldLayout Horizontal "First:"     firstName editor
      lastName  <- fieldLayout Horizontal "Last:"      lastName editor
      return (firstName, lastName)
  (age, education) <- withLayout Vertical $ construct $ do
      age       <- fieldLayout Horizontal "Age:"       age editor
      education <- fieldLayout Horizontal "Education:" education editorEducation
      return (age, education)
  (status, brexiteer) <- withLayout Vertical $ construct $ do
      status    <- fieldLayout Horizontal "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Horizontal "Brexiter"   brexiteer editor
      return (status, brexiteer)
  return Person{..}


editorPersonVertical = construct $ do
  (firstName, lastName, age) <- withLayout Horizontal $ construct $ do
      firstName <- fieldLayout Vertical "First:"     firstName editor
      lastName  <- fieldLayout Vertical "Last:"      lastName editor
      age       <- fieldLayout Vertical "Age:"       age editor
      return (firstName, lastName, age)
  (education, status, brexiteer) <- withLayout Horizontal $ construct $ do
      education <- fieldLayout Vertical "Education:" education editorEducation
      status    <- fieldLayout Vertical "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Vertical "Brexiter"   brexiteer editor
      return (education, status, brexiteer)
  return Person{..}

editorPersonColumns = do
      firstName <- fieldLayout Next "First:"     firstName editor
      lastName  <- fieldLayout Next "Last:"      lastName editor
      age       <- fieldLayout Next "Age:"       age editor
      education <- fieldLayout Break "Education:" education editorEducation
      status    <- fieldLayout Next "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Next "Brexiter"   brexiteer editor
      return Person{..}

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1H <- createEditor editorPersonHorizontal person1B
  person1V <- createEditor editorPersonVertical   person1B
  person1C_ <- runEF editorPersonColumns person1B
  let person1Cl = _editorElement person1C_
  person1C <- createEditor (construct editorPersonColumns) person1B
  person2 <- createEditor editorGeneric person1B
  person1B <- stepper def (head <$> unions [edited person1H, edited person1V, edited person1C, edited person2])

  getBody w #+ [grid
    [ [return $ _editorElement person1H]
    , [hr]
    , [return $ _editorElement person1V]
    , [hr]
    , [new # set text (show person1Cl)]
    , [hr]
    , [return $ _editorElement person1C]
    , [hr]
    , [return $ _editorElement person2]
    , [hr]
    ]]
