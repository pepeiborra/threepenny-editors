{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Person (main) where
import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Generics.SOP                              as SOP
import           Generics.SOP.TH
import           GHC.Generics                              (Generic)
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Editors
import           Graphics.UI.Threepenny.Editors.Layout     (above, beside)
import           Graphics.UI.Threepenny.Editors.Validation
import           Graphics.UI.Threepenny.Elements
import           Prelude                                   hiding (span)

main :: IO ()
main = startGUI defaultConfig setup

-- | A dual purpose data type that doubles as a value and as a widget depending on the type argument.
data PersonF (purpose :: Purpose) = Person
  { education           :: Field purpose Education
  , firstName, lastName :: Field purpose Text
  , age                 :: Field purpose (Maybe Int)
  , brexiteer           :: Field purpose Brexiteer
  , status              :: Field purpose LegalStatus
  }
  deriving (Generic)

type Person = PersonF Data
type PersonEditor = PersonF Edit

instance Validable Person where
  validate Person{..} = fromWarnings $
    [ "First name cannot be null" | T.null firstName ] ++
    [ "Last name cannot be null"  | T.null lastName ] ++
    [ "Age must be a natural number" | Just x <- [age], x <= 0]

data LegalStatus
  = Single
  | Married
  | Divorced
  | Widowed
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance HasEmpty LegalStatus
instance Editable LegalStatus
instance SOP.HasDatatypeInfo LegalStatus
instance SOP.Generic LegalStatus

data Education
  = Basic
  | Intermediate
  | Other Text
  deriving (Eq, Ord, Read, Show, Generic)

getOther :: Education -> Maybe Text
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
      , ("Other", dimapE (fromMaybe mempty . getOther) Other someEditor)
      ]
      selector

instance HasEmpty Education
instance Editable Education
instance SOP.HasDatatypeInfo Education
instance SOP.Generic Education

newtype Brexiteer = Brexiteer Bool deriving (Eq, Show, Ord, Generic)

instance HasEmpty Brexiteer
instance Editable Brexiteer where editor = editorGeneric
instance SOP.HasDatatypeInfo Brexiteer
instance SOP.Generic Brexiteer

deriving instance Show Person

instance HasEmpty Person
instance Editable Person where editor = editorGeneric

-- | An editor for 'Person' values that uses the 'Columns' layout builder
editorPersonColumns :: Editor Person Columns Person
editorPersonColumns = do
      firstName <- fieldLayout Next "First:"     firstName editor
      lastName  <- fieldLayout Next "Last:"      lastName editor
      age       <- fieldLayout Next "Age:"       age editor
      education <- fieldLayout Break "Education:" education editorEducation
      status    <- fieldLayout Next "Status"     status (editorJust $ editorSelection (pure [minBound..]) (pure (string.show)))
      brexiteer <- fieldLayout Next "Brexiter"   brexiteer editor
      return Person{..}


-- | A editor for 'Person' values with a fully fledged Widget type.
--   The UI and layout are defined in the 'Renderable' instance for the widget.
personEditor :: Editor Person PersonEditor Person
personEditor = editorGenericBi

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

-- -------
-- Driver

defPerson :: Person
defPerson = Person Basic (T.pack "First") (T.pack "Last") Nothing (Brexiteer False) Single

setup :: Window -> UI ()
setup w = void $ mdo
  _ <- return w # set title "Threepenny editors example"
  person1C <- create editorPersonColumns person1B
  person2  <- create editorGeneric person1B
  person3e <- create personEditor person1B
  -- When using a biapplicative editor, we can set the attributes of the field editors after creation.
  _ <- element (firstName (widgetControl person3e)) # set style [("background-color", "Blue")]
  person1B <- accumB defPerson (updateIfValid . head <$> unions
                            [ edited person1C
                            , edited person2
                            , edited person3e
                            ])

  -- We can attach validation to any editor
  validation <-
      stepper ok (validate . head <$>
                  unions [ edited person1C
                         , edited person2
                         , edited person3e])

  getBody w #+ [grid
    [ [span # sink text (show <$> validation) # set style [("color", "red")]]
    , [render person1C]
    , [hr]
    , [render person2]
    , [hr]
    , [render person3e]
    , [hr]
    ]]

deriveGeneric ''PersonF
