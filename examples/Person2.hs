{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Person2 where

import           Control.Monad
import           Generics.SOP.TH
import           Graphics.UI.Threepenny
import           Graphics.UI.Threepenny.Editors

data Education
  = Basic
  | Intermediate
  | Other String
  deriving Show

instance Editable Education
instance HasEmpty Education

newtype Brexiteer = Brexiteer Bool deriving Show

instance Editable Brexiteer where editor = editorGeneric
instance HasEmpty Brexiteer

data LegalStatus
  = Single
  | Married
  | Divorced
  | Widowed
  deriving Show

instance HasEmpty LegalStatus
instance Editable LegalStatus

data PersonDual (purpose :: Purpose) = Person
  { education           :: Field purpose Education
  , firstName, lastName :: Field purpose String
  , age                 :: Field purpose (Maybe Int)
  , brexiteer           :: Field purpose Brexiteer
  , status              :: Field purpose LegalStatus
  , addresses           :: ListField purpose String
 -- , children            :: ListField purpose Person
  }

deriving instance Show Person

type Person = PersonDual 'Data
type PersonEditor = PersonDual 'Edit

instance HasEmpty Person

instance Editable Person where
  type EditorWidget Person = PersonEditor
  editor = editorGenericBi

instance Renderable PersonEditor where
  render = renderGeneric

deriveGeneric ''Education
deriveGeneric ''Brexiteer
deriveGeneric ''LegalStatus
deriveGeneric ''PersonDual

main :: IO ()
main = startGUI defaultConfig $ \w -> mdo
  personE :: GenericWidget PersonEditor Person <- create editor personB
  personB :: Behavior Person <- stepper emptyValue (edited personE)

  void $ getBody w #+ [render personE]
  void $ element (firstName (widgetControl personE)) # set style [("background-color", "Blue")]
