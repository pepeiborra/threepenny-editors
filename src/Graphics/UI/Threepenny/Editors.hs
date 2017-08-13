{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-duplicate-exports  #-}

module Graphics.UI.Threepenny.Editors
  ( -- * Widgets
    GenericWidget(..)
  , edited
  , contents
  , widgetControl
  , widgetTidings
    -- * Editors
  , Editor(Horizontally, horizontally, Vertically, vertically)
  , someEditor
  , create
  , lmapE
  , dimapE
  , Editable(..)
  , EditorWidgetFor(..)
  , Field
  , Usage(..)
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , fieldLayout
  , pattern Horizontally
  , pattern Vertically
    -- ** Editor layout
  , withLayout
  , withSomeWidget
  , construct
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
    -- ** Generic editors
  , editorGeneric
  , editorGenericSimple
    -- * Layouts
  , Layout
  , above
  , beside
  -- ** Monoidal layouts
  , Vertical(..)
  , Horizontal(..)
  , Columns(..)
  -- ** Type level layouts
  , type (|*|)(..)
  , type (-*-)(..)
  -- ** Layout manipulation
  , Renderable(..)
  ) where

import           Data.Biapplicative
import           Data.Char
import           Data.Default
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Maybe
import           Generics.SOP                          hiding (Compose)
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Widgets
import           Text.Casing

import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Types

-- | The class of 'Editable' datatypes.
--   .
--   Define your own instance by using the 'Applicative' composition operators or
--   derive it via 'Generics.SOP'.
class Renderable (EditorWidget a) => Editable a where
  type family EditorWidget a
  type EditorWidget a = Layout
  -- | The editor factory
  editor :: Editor a (EditorWidget a) a
  default editor :: (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a)), EditorWidget a ~ Layout) => Editor a (EditorWidget a) a
  editor = editorGeneric

-- | Conceal the widget type of some 'Editor'
withSomeWidget :: Renderable w => Editor a w b -> Editor a Layout b
withSomeWidget = first getLayout

-- | A version of 'editor' with a concealed widget type.
someEditor :: Editable a => Editor a Layout a
someEditor = withSomeWidget editor

-- | A container for 'EditorWidget'.
data EditorWidgetFor a where
  EditorWidgetFor :: Editable a => EditorWidget a -> EditorWidgetFor a

-- | 'Usage' is a kind for type level 'Field's
data Usage = Value | Edit

-- | Type level fields. Use this helper to define EditorWidget types. Example:
--
-- > data PersonF (usage :: Usage) = Person
-- >   { education           :: Field usage Education
-- >   , firstName, lastName :: Field usage String
-- >   , age                 :: Field usage (Maybe Int)
--
-- > type Person = PersonF Value
-- > type PersonEditor = PersonF Edit
type family Field (usage :: Usage) a where
  Field 'Value  a = a
  Field 'Edit a = EditorWidget a

instance Editable () where
  type EditorWidget () = Element
  editor = editorUnit

instance a ~ Char => Editable [a] where
  type EditorWidget [a] = TextEntry
  editor = editorString

instance Editable Bool where
  type EditorWidget Bool = Element
  editor = editorCheckBox

instance Editable (Maybe Int) where
  type EditorWidget (Maybe Int) = TextEntry
  editor = editorReadShow
instance Editable (Maybe Double) where
  type EditorWidget (Maybe Double) = TextEntry
  editor = editorReadShow
instance Editable Int where
  type EditorWidget Int = TextEntry
  editor = editorJust editor
instance Editable Double where
  type EditorWidget Double = TextEntry
  editor = editorJust editor

instance (Editable a, Editable b) => Editable (a,b) where
  type EditorWidget (a,b) = EditorWidget a |*| EditorWidget b
  editor = bipure (:|*|) (,) <<*>> lmapE fst editor <<*>> lmapE snd editor

instance Editable a => Editable (Identity a) where
  type EditorWidget (Identity a) = EditorWidget a
  editor = editorIdentity editor

{--------------------------------------------
  Generic derivations
---------------------------------------------}
-- | A generic editor for record types.
editorGenericSimple
  :: forall a xs.
     (Generic a, HasDatatypeInfo a, All Editable xs, Code a ~ '[xs])
  => Editor a Layout a
editorGenericSimple = dimapE from to $ editorGenericSimple' (datatypeInfo(Proxy @ a))

editorGenericSimple'
  :: forall xs.
     (All Editable xs)
  => DatatypeInfo '[xs] -> Editor (SOP I '[xs]) Layout (SOP I '[xs])
editorGenericSimple' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGenericSimple' (Newtype _ _ c)      = constructorEditorFor c

constructorEditorFor
  :: (All Editable xs)
  => ConstructorInfo xs
  -> Editor (SOP I '[xs]) Layout (SOP I '[xs])
constructorEditorFor (Record _ fields) = dimapE (unZ . unSOP) (SOP . Z) $ constructorEditorFor' fields
constructorEditorFor (Constructor _) = dimapE (unZ . unSOP) (SOP . Z) someEditor
constructorEditorFor Infix{} = dimapE (unZ . unSOP) (SOP . Z) someEditor

-- | A generic editor for SOP types.
editorGeneric
  :: forall a .
     (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a)))
  => Editor a Layout a
editorGeneric = dimapE from to $ editorGeneric' (datatypeInfo(Proxy @ a))

editorGeneric'
  :: forall xx.
     (All (All Editable `And` All Default) xx)
  => DatatypeInfo xx -> Editor (SOP I xx) Layout (SOP I xx)
editorGeneric' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGeneric' (ADT _ _ cc) = editorSum above editors constructor where
  editors :: [(Tag, Editor (SOP I xx) Layout (SOP I xx))]
  editors = first Tag <$> constructorEditorsFor cc
  constructors = hmap (K . constructorName) cc
  constructor a = Tag $ hcollapse $ hliftA2 const constructors (unSOP a)
editorGeneric' (Newtype _ _ c) = constructorEditorFor c

newtype Tag = Tag String deriving (Eq, Ord)
instance Show Tag where show (Tag t) = init $ toFieldLabel t

constructorEditorsFor
  :: forall xx . (All (All Editable `And` All Default) xx)
  => NP ConstructorInfo xx -> [(String, Editor (SOP I xx) Layout (SOP I xx))]
constructorEditorsFor cc =
  hcollapse $ hcliftA3 p (\c i p -> (constructorName c,) `mapKK` constructorEditorForUnion c i p) cc
    (injections  :: NP (Injection  (NP I) xx) xx)
    (projections :: NP (Projection (Compose Maybe (NP I)) xx) xx)
  where
    p = Proxy @ (All Editable `And` All Default)

constructorEditorForUnion
  :: (SListI xx, All Editable xs, All Default xs)
  => ConstructorInfo xs
  -> Injection (NP I) xx xs
  -> Projection (Compose Maybe (NP I)) xx xs
  -> K (Editor (SOP I xx) Layout (SOP I xx)) xs
constructorEditorForUnion (Constructor _) inj prj = K $ composeEditor inj prj editor
constructorEditorForUnion Infix{} inj prj = K $ composeEditor inj prj editor
constructorEditorForUnion (Record _ fields) inj prj = K $ composeEditor inj prj $ constructorEditorFor' fields

composeEditor
  :: forall xss xs.
    (SListI xss, All Default xs) =>
     Injection (NP I) xss xs
  -> Projection (Compose Maybe (NP I)) xss xs
  -> Editor (NP I xs) Layout (NP I xs)
  -> Editor (SOP I xss) Layout (SOP I xss)
composeEditor (Fn inj) (Fn prj) = dimapE f (SOP . unK . inj)
  where
    f :: SOP I xss -> NP I xs
    f = fromMaybe def . getCompose . prj . K . hexpand (Compose Nothing) . hmap (Compose . Just) . unSOP

constructorEditorFor' :: (SListI xs, All Editable xs) => NP FieldInfo xs -> Editor (NP I xs) Layout (NP I xs)
constructorEditorFor' fields = vertically $ hsequence $ hliftA Vertically $ fieldsEditor (hliftA (K . fieldName) fields)

-- | Tuple editor without fields
instance All Editable xs => Editable (NP I xs) where
  type EditorWidget (NP I xs) = Layout
  editor = horizontally $ hsequence $ hliftA Horizontally tupleEditor

tupleEditor :: forall xs . All Editable xs => NP (Editor (NP I xs) Layout) xs
tupleEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (Editor (NP I xs) Layout) ys
  go _ SNil  = Nil
  go f SCons = lmapE (unI . hd . f) someEditor :* go (tl . f) sList

fieldsEditor :: forall xs . All Editable xs => NP (K String) xs -> NP (Editor (NP I xs) Layout) xs
fieldsEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (K String) ys -> NP (Editor (NP I xs) Layout) ys
  go _ SNil Nil = Nil
  go f SCons (K fn :* xs) = field (toFieldLabel fn) (unI . hd . f) someEditor :* go (tl . f) sList xs

toFieldLabel :: String -> String
toFieldLabel (fromAny -> Identifier (x:xx)) =
  unwords (onHead toUpper x : fmap (onHead toLower) xx) ++ ":"
    where
      onHead f (x:xx) = f x : xx
      onHead _ []     = []
toFieldLabel _ = ""

instance (Applicative f, All Default xs) => Default (NP f xs) where
  def = hcpure (Proxy @ Default) (pure def)
