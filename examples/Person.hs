{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Person (main) where
import           Control.Monad
import           Data.Biapplicative
import           Data.Default
import           Data.Maybe
import qualified Generics.SOP                              as SOP
import           GHC.Generics                              (Generic)
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Editors
import           Graphics.UI.Threepenny.Elements
import           Prelude                                   hiding (span)

main :: IO ()
main = startGUI defaultConfig setup

-- | A dual purpose data type that doubles as a value and as a widget depending on the type argument.
data PersonF (usage :: Usage) = Person
  { education           :: Field usage Education
  , firstName, lastName :: Field usage String
  , age                 :: Field usage (Maybe Int)
  , brexiteer           :: Field usage Brexiteer
  , status              :: Field usage LegalStatus
  }
  deriving (Generic)

type Person = PersonF Value
type PersonEditor = PersonF Edit

instance Validable Person where
  validate Person{..} = fromWarnings $ 
    [ "First name cannot be null" | null firstName ] ++
    [ "Last name cannot be null"  | null lastName ] ++
    [ "Age must be a natural number" | Just x <- [age], x <= 0]

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

-- | A manually defined editor for 'Education'.
--   It is also possible to derive this 'Editor' via Generics.SOP, as done below.
editorEducation :: Editor Education Layout Education
editorEducation = do
    let selector x = case x of
            Other _ -> "Other"
            _       -> show x
    editorSum beside
      [ ("Basic", const Basic <$> withSomeWidget editorUnit)
      , ("Intermediate", const Intermediate <$> withSomeWidget editorUnit)
      , ("Other", dimapE (fromMaybe "" . getOther) Other someEditor)
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

deriving instance Show Person

instance Editable Person
instance SOP.HasDatatypeInfo Person
instance SOP.Generic Person
instance Default Person where def = Person Basic "First" "Last" (Just 18) def def

-- | An editor for 'Person' values that combines the 'Horizontal' and 'Vertical' layout builders
editorPersonHV :: Editor Person Vertical Person
editorPersonHV = do
  (firstName, lastName) <- withLayout Vertical $ construct $ do
      firstName <- fieldLayout Horizontal "First:"     firstName editor
      lastName  <- fieldLayout Horizontal "Last:"      lastName editor
      return (firstName, lastName)
  (age, education) <- withLayout Vertical $ construct $ do
      age       <- fieldLayout Horizontal "Age:"       age editor
      education <- fieldLayout Horizontal "Education:" education editorEducation
      return (age, education)
  (status, brexiteer) <- withLayout Vertical $ construct $ do
      status    <- fieldLayout Horizontal "Status"     status (withSomeWidget $ editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Horizontal "Brexiter"   brexiteer editor
      return (status, brexiteer)
  return Person{..}

-- | An editor for 'Person' values that uses the 'Columns' layout builder
editorPersonColumns :: Editor Person Columns Person
editorPersonColumns = do
      firstName <- fieldLayout Next "First:"     firstName editor
      lastName  <- fieldLayout Next "Last:"      lastName editor
      age       <- fieldLayout Next "Age:"       age editor
      education <- fieldLayout Break "Education:" education editorEducation
      status    <- fieldLayout Next "Status"     status (withSomeWidget $ editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Next "Brexiter"   brexiteer editor
      return Person{..}


-- | A editor for 'Person' values with a fully fledged Widget type.
--   The UI and layout are defined in the 'Renderable' instance for the widget.
personEditor :: Editor Person PersonEditor Person
personEditor =
    bipure Person Person
      <<*>> edit education editor
      <<*>> edit firstName editor
      <<*>> edit lastName  editor
      <<*>> edit age       editor
      <<*>> edit brexiteer editor
      <<*>> edit status    editor

instance Renderable PersonEditor where
  getLayout Person{..} =
    ( ("First: "  ||| firstName) ===
      ("Last: "   ||| lastName)  ===
      ("Status: " ||| status)
    ) |||
    (("Age:" ||| age) ===
     ("Brexiteer: " ||| brexiteer) ===
     ("Education: " ||| education))
   where
    a ||| b = getLayout a `beside` getLayout b
    a === b = getLayout a `above`  getLayout b

-- Driver
setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1HV <- create editorPersonHV person1B
  person1C <- create editorPersonColumns person1B
  person2  <- create editorGeneric person1B
  person3e <- create personEditor person1B
  -- When using a biapplicative editor, we can set the attributes of the field editors after creation.
  _ <- element (firstName (widgetControl person3e)) # set style [("background-color", "Blue")]
  person1B <- accumB def (updateIfValid . head <$> unions
                            [ edited person1HV
                            , edited person1C
                            , edited person2
                            , edited person3e
                            ])

  -- We can attach validation to any editor
  validation <-
      stepper ok (validate . head <$>
                  unions [ edited person1HV
                          , edited person1C
                          , edited person2
                          , edited person3e])

  getBody w #+ [grid
    [ [span # sink text (show <$> validation) # set style [("color", "red")]]
    , [render person1HV]
    , [hr]
    , [render person1C]
    , [hr]
    , [render person2]
    , [hr]
    , [render person3e]
    , [hr]
    ]]
