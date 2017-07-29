{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , editorElement
  , editorTidings
  , EditorFactory
  , createEditor
  , Editable(..)
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , Vertically(..)
  , Horizontally(..)
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
    -- ** Layouts
  , Layout(Grid, Single)
  , horizontal
  , vertical
  , runLayout
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Default
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Profunctor
import           Generics.SOP                          hiding (Compose)
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.Widgets
import           Text.Casing
import           Text.Read

import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Types

-- | The class of 'Editable' datatypes.
--   .
--   Define your own instance by using the 'Applicative' composition operators or
--   derive it via 'Generics.SOP'.
class Editable a where
  -- | The editor factory
  editor :: EditorFactory Layout a a
  default editor :: (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a))) => EditorFactory Layout a a
  editor = editorGeneric

editorReadShow :: (Read a, Show a) => EditorFactory Layout (Maybe a) (Maybe a)
editorReadShow = EF $ \b -> do
    e <- runEF editor (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> edited e)
    return $ Editor t (_editorElement e)

instance Editable () where
  editor = EF $ \b -> do
    t <- new
    return $ Editor (tidings b never) (Single t)

instance a ~ Char => Editable [a] where
  editor = EF $ \b -> do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ Editor (userText t) (Single $ getElement t)

instance Editable Bool where
  editor = EF $ \b -> do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ Editor (tidings b $ checkedChange t) (Single t)

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow
instance Editable Int where editor = editorJust editor
instance Editable Double where editor = editorJust editor

instance (Editable a, Editable b) => Editable (a,b) where
  editor = (,) <$> lmap fst editor |*| lmap snd editor

instance Editable a => Editable (Identity a) where
  editor = editorIdentity editor

{--------------------------------------------
  Generic derivations
---------------------------------------------}
-- | A generic editor for record types.
editorGenericSimple
  :: forall a xs.
     (Generic a, HasDatatypeInfo a, All Editable xs, Code a ~ '[xs])
  => EditorFactory Layout a a
editorGenericSimple = dimap from to $ editorGenericSimple' (datatypeInfo(Proxy @ a))

editorGenericSimple'
  :: forall xs.
     (All Editable xs)
  => DatatypeInfo '[xs] -> EditorFactory Layout (SOP I '[xs]) (SOP I '[xs])
editorGenericSimple' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGenericSimple' (Newtype _ _ c)      = constructorEditorFor c

constructorEditorFor
  :: (All Editable xs)
  => ConstructorInfo xs
  -> EditorFactory Layout (SOP I '[xs]) (SOP I '[xs])
constructorEditorFor (Record _ fields) = dimap (unZ . unSOP) (SOP . Z) $ constructorEditorFor' fields
constructorEditorFor (Constructor _) = dimap (unZ . unSOP) (SOP . Z) editor
constructorEditorFor Infix{} = dimap (unZ . unSOP) (SOP . Z) editor

-- | A generic editor for SOP types.
editorGeneric
  :: forall a .
     (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a)))
  => EditorFactory Layout a a
editorGeneric = dimap from to $ editorGeneric' (datatypeInfo(Proxy @ a))

editorGeneric'
  :: forall xx.
     (All (All Editable `And` All Default) xx)
  => DatatypeInfo xx -> EditorFactory Layout (SOP I xx) (SOP I xx)
editorGeneric' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGeneric' (ADT _ _ cc) = editorSum vertical editors constructor where
  editors :: [(Tag, EditorFactory Layout (SOP I xx) (SOP I xx))]
  editors = first Tag <$> constructorEditorsFor cc
  constructors = hmap (K . constructorName) cc
  constructor a = Tag $ hcollapse $ hliftA2 const constructors (unSOP a)
editorGeneric' (Newtype _ _ c) = constructorEditorFor c

newtype Tag = Tag String deriving (Eq, Ord)
instance Show Tag where show (Tag t) = init $ toFieldLabel t

constructorEditorsFor
  :: forall xx . (All (All Editable `And` All Default) xx)
  => NP ConstructorInfo xx -> [(String, EditorFactory Layout (SOP I xx) (SOP I xx))]
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
  -> K (EditorFactory Layout (SOP I xx) (SOP I xx)) xs
constructorEditorForUnion (Constructor _) inj prj = K $ composeEditorFactory inj prj editor
constructorEditorForUnion Infix{} inj prj = K $ composeEditorFactory inj prj editor
constructorEditorForUnion (Record _ fields) inj prj = K $ composeEditorFactory inj prj $ constructorEditorFor' fields

composeEditorFactory
  :: forall xss xs.
    (SListI xss, All Default xs) =>
     Injection (NP I) xss xs
  -> Projection (Compose Maybe (NP I)) xss xs
  -> EditorFactory Layout (NP I xs) (NP I xs)
  -> EditorFactory Layout (SOP I xss) (SOP I xss)
composeEditorFactory (Fn inj) (Fn prj) = dimap f (SOP . unK . inj)
  where
    f :: SOP I xss -> NP I xs
    f = fromMaybe def . getCompose . prj . K . hexpand (Compose Nothing) . hmap (Compose . Just) . unSOP

constructorEditorFor' :: (SListI xs, All Editable xs) => NP FieldInfo xs -> EditorFactory Layout (NP I xs) (NP I xs)
constructorEditorFor' fields = vertically $ hsequence $ hliftA Vertically $ fieldsEditor (hliftA (K . fieldName) fields)

-- | Tuple editor without fields
instance All Editable xs => Editable (NP I xs) where
  editor = horizontally $ hsequence $ hliftA Horizontally tupleEditor

tupleEditor :: forall xs . All Editable xs => NP (EditorFactory Layout (NP I xs)) xs
tupleEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (EditorFactory Layout (NP I xs)) ys
  go _ SNil  = Nil
  go f SCons = lmap (unI . hd . f) editor :* go (tl . f) sList

fieldsEditor :: forall xs . All Editable xs => NP (K String) xs -> NP (EditorFactory Layout (NP I xs)) xs
fieldsEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (K String) ys -> NP (EditorFactory Layout (NP I xs)) ys
  go _ SNil Nil = Nil
  go f SCons (K fn :* xs) = field (toFieldLabel fn) (unI . hd . f) editor :* go (tl . f) sList xs

toFieldLabel :: String -> String
toFieldLabel (fromAny -> Identifier (x:xx)) =
  unwords (onHead toUpper x : fmap (onHead toLower) xx) ++ ":"
    where
      onHead f (x:xx) = f x : xx
      onHead _ []     = []
toFieldLabel _ = ""

instance (Applicative f, All Default xs) => Default (NP f xs) where
  def = hcpure (Proxy @ Default) (pure def)
