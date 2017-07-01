{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS -Wno-orphans #-}

module Graphics.UI.Threepenny.Editors.Profunctor
  ( -- * Editors
    Base.Editor(..)
  , Base.edited
  , Base.contents
  , EditorFactory(..)
  , createEditor
  , liftEditor
  , Editable(..)
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
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
  )where

import           Data.Bifunctor
import           Data.Char
import           Data.Default
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Profunctor
import           Data.Proxy
import           Generics.SOP hiding (Compose)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Editors.Base as Base
import           Text.Casing

-- | A function from 'Behavior' @a@ to 'Editor' @a@
newtype EditorFactory a b = EditorFactory
  { run :: Behavior a -> Compose UI Base.EditorDef b
  }

liftEditor :: (UI Element -> UI Element) -> EditorFactory a b -> EditorFactory a b
liftEditor f (EditorFactory run) = EditorFactory $ \b ->
  case run b of
    Compose uidef -> Compose $ fmap (Base.liftEditorDef f) uidef

-- | Create an editor to display the argument.
--   User edits are fed back via the 'edited' 'Event'.
createEditor :: EditorFactory b a -> Behavior b -> UI (Base.Editor a)
createEditor e b = getCompose (run e b) >>= Base.runEditorDef

instance Functor (EditorFactory a) where
  fmap = dimap id

instance Profunctor EditorFactory where
  dimap g h (EditorFactory f) = EditorFactory $ \b -> h <$> f (g <$> b)


-- | The class of 'Editable' datatypes.
--   .
--   Define your own instance by using the 'Applicative' composition operators or
--   derive it via 'Generics.SOP'.
class Editable a where
  editor :: EditorFactory a a
  default editor :: (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a))) => EditorFactory a a
  editor = editorGeneric

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Horizontal applicative composition.
(|*|) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
a |*| b = EditorFactory $ \s -> run a s Base.|*| run b s

(|*) :: EditorFactory s a -> UI Element -> EditorFactory s a
a |* e = EditorFactory $ \s -> run a s Base.|* e

(*|) :: UI Element -> EditorFactory s a -> EditorFactory s a
e *| a = EditorFactory $ \s -> e Base.*| run a s

-- | Vertical applicative composition.
(-*-) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
a -*- b = EditorFactory $ \s -> run a s Base.-*- run b s

(-*) :: EditorFactory s a -> UI Element -> EditorFactory s a
a -* e = EditorFactory $ \s -> run a s Base.-* e

(*-) :: UI Element -> EditorFactory s a -> EditorFactory s a
e *- a = EditorFactory $ \s -> e Base.*- run a s

-- | A helper that arranges a label with the field name
--   and the editor horizontally.
field :: String -> (out -> inn) -> EditorFactory inn a -> EditorFactory out a
field name f e = string name *| lmap f e

editorUnit :: EditorFactory a ()
editorUnit = EditorFactory $ \_ -> Base.editor (pure ())

-- | An editor that presents a free form input.
editorReadShow :: (Read a, Show a) => EditorFactory (Maybe a) (Maybe a)
editorReadShow = EditorFactory Base.editorReadShow

-- | An editor that presents a choice of values.
editorEnumBounded
  :: (Show a, Ord a, Enum a, Bounded a)
  => Behavior (a -> UI Element) -> EditorFactory (Maybe a) (Maybe a )
editorEnumBounded display = EditorFactory $ Base.editorEnumBounded display

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: EditorFactory (Maybe a) (Maybe a) -> EditorFactory a a
editorJust e = EditorFactory $ Base.editorJust (run e)

-- | An editor that presents a dynamic choice of values.
editorSelection :: Ord a => Behavior [a] -> Behavior(a -> UI Element) -> EditorFactory (Maybe a) (Maybe a)
editorSelection opts displ = EditorFactory $ Base.editorSelection opts displ

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Show tag, Ord tag)
  => [(tag, EditorFactory b b)] -> (b -> tag) -> EditorFactory b b
editorSum nested tagger = EditorFactory $ \b ->
  let nested' = [ (tag, run f b) | (tag, f) <- nested ]
  in Base.editorSum nested' tagger b

instance Editable () where editor = EditorFactory Base.editor
instance Editable String where editor = EditorFactory Base.editor
instance Editable Bool where editor = EditorFactory Base.editor
instance Editable Int where editor = EditorFactory Base.editor
instance Editable Double where editor = EditorFactory Base.editor
instance Editable (Maybe Int) where editor = EditorFactory Base.editor
instance Editable (Maybe Double) where editor = EditorFactory Base.editor

instance (Editable a, Editable b) => Editable (a,b) where
  editor = (,) <$> lmap fst editor |*| lmap snd editor

instance Editable a => Editable (Identity a) where
  editor = editorIdentity editor

editorIdentity :: EditorFactory a a -> EditorFactory (Identity a) (Identity a)
editorIdentity = dimap runIdentity Identity

{--------------------------------------------
  Generic derivations
---------------------------------------------}
-- | A generic editor for record types.
editorGenericSimple
  :: forall a xs.
     (Generic a, HasDatatypeInfo a, All Editable xs, Code a ~ '[xs])
  => EditorFactory a a
editorGenericSimple = dimap from to $ editorGenericSimple' (datatypeInfo(Proxy @ a))

editorGenericSimple'
  :: forall xs.
     (All Editable xs)
  => DatatypeInfo '[xs] -> EditorFactory (SOP I '[xs]) (SOP I '[xs])
editorGenericSimple' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGenericSimple' (Newtype _ _ c) = constructorEditorFor c

constructorEditorFor
  :: (All Editable xs)
  => ConstructorInfo xs
  -> EditorFactory (SOP I '[xs]) (SOP I '[xs])
constructorEditorFor (Record _ fields) = dimap (unZ . unSOP) (SOP . Z) $ constructorEditorFor' fields
constructorEditorFor (Constructor _) = dimap (unZ . unSOP) (SOP . Z) editor
constructorEditorFor Infix{} = dimap (unZ . unSOP) (SOP . Z) editor

-- | A generic editor for SOP types.
editorGeneric
  :: forall a .
     (Generic a, HasDatatypeInfo a, (All (All Editable `And` All Default) (Code a)))
  => EditorFactory a a
editorGeneric = dimap from to $ editorGeneric' (datatypeInfo(Proxy @ a))

editorGeneric'
  :: forall xx.
     (All (All Editable `And` All Default) xx)
  => DatatypeInfo xx -> EditorFactory (SOP I xx) (SOP I xx)
editorGeneric' (ADT _ _ (c :* Nil)) = constructorEditorFor c
editorGeneric' (ADT _ _ cc) = editorSum editors constructor where
  editors :: [(Tag, EditorFactory (SOP I xx) (SOP I xx))]
  editors = map (first Tag) $ constructorEditorsFor cc
  constructors = hmap (K . constructorName) cc
  constructor a = Tag $ hcollapse $ hliftA2 const constructors (unSOP a)
editorGeneric' (Newtype _ _ c) = constructorEditorFor c

newtype Tag = Tag String deriving (Eq, Ord)
instance Show Tag where show (Tag t) = t

constructorEditorsFor
  :: forall xx . (All (All Editable `And` All Default) xx)
  => NP ConstructorInfo xx -> [(String, EditorFactory (SOP I xx) (SOP I xx))]
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
  -> K (EditorFactory (SOP I xx) (SOP I xx)) xs
constructorEditorForUnion (Constructor _) inj prj = K $ composeEditorFactory inj prj editor
constructorEditorForUnion Infix{} inj prj = K $ composeEditorFactory inj prj editor
constructorEditorForUnion (Record _ fields) inj prj = K $ composeEditorFactory inj prj $ constructorEditorFor' fields

composeEditorFactory
  :: forall xss xs.
    (SListI xss, All Default xs) =>
     Injection (NP I) xss xs
  -> Projection (Compose Maybe (NP I)) xss xs
  -> EditorFactory (NP I xs) (NP I xs)
  -> EditorFactory (SOP I xss) (SOP I xss)
composeEditorFactory (Fn inj) (Fn prj) = dimap f (SOP . unK . inj)
  where
    f :: SOP I xss -> NP I xs
    f = fromMaybe def . getCompose . prj . K . hexpand (Compose Nothing) . hmap (Compose . Just) . unSOP

constructorEditorFor' :: (SListI xs, All Editable xs) => NP FieldInfo xs -> EditorFactory (NP I xs) (NP I xs)
constructorEditorFor' fields = unVEF $ hsequence $ hliftA VEF $ fieldsEditor (hliftA (K . fieldName) fields)

-- | Tuple editor without fields
instance All Editable xs => Editable (NP I xs) where
  editor = unHEF $ hsequence $ hliftA HEF tupleEditor

tupleEditor :: forall xs . All Editable xs => NP (EditorFactory (NP I xs)) xs
tupleEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (EditorFactory (NP I xs)) ys
  go _ SNil = Nil
  go f SCons = lmap (unI . hd . f) editor :* go (tl . f) sList

fieldsEditor :: forall xs . All Editable xs => NP (K String) xs -> NP (EditorFactory (NP I xs)) xs
fieldsEditor = go id sList where
  go :: forall ys. All Editable ys => (forall f . NP f xs -> NP f ys) -> SList ys -> NP (K String) ys -> NP (EditorFactory (NP I xs)) ys
  go _ SNil Nil = Nil
  go f SCons (K fn :* xs) = field (toFieldLabel fn) (unI . hd . f) editor :* go (tl . f) sList xs

toFieldLabel :: String -> String
toFieldLabel (fromAny -> Identifier (x:xx)) =
  unwords (onHead toUpper x : map (onHead toLower) xx) ++ ":"
    where
      onHead f (x:xx) = f x : xx
      onHead _ [] = []
toFieldLabel _ = ""

-- | EditorFactory with an Applicative instance for vertical composition
newtype VEF a b = VEF {unVEF :: EditorFactory a b} deriving (Functor, Profunctor)
instance Applicative (VEF a) where
  pure x = VEF $ const x <$> editorUnit
  VEF a <*> VEF b = VEF (a -*- b)

-- | EditorFactory with an Applicative instance for horizontal composition
newtype HEF a b = HEF {unHEF :: EditorFactory a b} deriving (Functor, Profunctor)
instance Applicative (HEF a) where
  pure x = HEF $ const x <$> editorUnit
  HEF a <*> HEF b = HEF (a |*| b)

instance (Applicative f, All Default xs) => Default (NP f xs) where
  def = hcpure (Proxy @ Default) (pure def)
