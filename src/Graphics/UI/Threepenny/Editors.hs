{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC  #-}
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
    -- ** Editor compoosition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorReadShow
  , editorEnumBounded
  , withDefault
  )where

import           Data.Maybe
import           Data.Profunctor
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.Widgets
import           Text.Read

data Editor a = Editor
  { editorTidings :: Tidings a
  , editorElement :: Element
  }
  deriving Functor

instance Widget (Editor a) where
  getElement = editorElement

-- | A newtype wrapper that provides a 'Profunctor' instance.
newtype EditorFactory a b = EditorFactory { run :: Behavior a -> UI (Editor b) }

instance Profunctor EditorFactory where
  dimap g h (EditorFactory f) = EditorFactory $ \b -> fmap h <$> f (g <$> b)

-- | The class of Editable datatypes.
class Editable a where
  -- | The editor factory
  editor :: Behavior a -> UI (Editor a)

edited :: Editor a -> Event a
edited = rumors . editorTidings

contents :: Editor a -> Behavior a
contents = facts . editorTidings

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Left-right editor composition
(|*|) :: UI(Editor (b -> a)) -> UI(Editor b) -> UI(Editor a)
a |*| b = do
  a <- a
  b <- b
  ab <- row [return $ getElement a, return $ getElement b]
  return $ Editor (editorTidings a <*> editorTidings b) ab

-- | Left-right composition of an element with a editor
(*|) :: UI Element -> UI (Editor a) -> UI (Editor a)
e *| a = do
  e <- e
  a <- a
  ea <- row [return e, return $ getElement a]
  return $ Editor (editorTidings a) ea

-- | Left-right composition of an element with a editor
(|*) :: UI (Editor a) -> UI Element -> UI (Editor a)
a |* e = do
  e <- e
  a <- a
  ea <- row [return $ getElement a, return e]
  return $ Editor (editorTidings a) ea

-- | Top-down editor composition
(-*-) :: UI(Editor (b -> a)) -> UI(Editor b) -> UI(Editor a)
a -*- b = do
  a <- a
  b <- b
  ab <- column [return $ getElement a, return $ getElement b]
  return $ Editor (editorTidings a <*> editorTidings b) ab

-- | Top-down composition of an element with a editor
(*-) :: UI Element -> UI (Editor a) -> UI (Editor a)
e *- a = do
  e <- e
  a <- a
  ea <- column [return e, return $ getElement a]
  return $ Editor (editorTidings a) ea

-- | Top-down composition of an element with a editor
(-*) :: UI (Editor a) -> UI Element -> UI (Editor a)
a -* e = do
  e <- e
  a <- a
  ea <- column [return $ getElement a, return e]
  return $ Editor (editorTidings a) ea

editorReadShow :: (Read a, Show a) => Behavior (Maybe a) -> UI (Editor (Maybe a))
editorReadShow b =
  do
    e <- editor (show <$> b)
    let t = tidings b (filterJust $ readMaybe <$> edited e)
    return $ Editor t (getElement e)

editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Behavior (Maybe a) -> UI (Editor (Maybe a))
editorEnumBounded display b = do
  l <- listBox (pure $ enumFrom minBound) b display
  return $ Editor (userSelection l) (getElement l)

withDefault
  :: EditorFactory (Maybe a) (Maybe b)
  -> b
  -> EditorFactory a b
withDefault editor def = dimap Just (fromMaybe def) editor

data SumWrapper tag a = A {display :: tag, factory :: Editor a}

instance Eq  tag  => Eq   (SumWrapper tag a) where A a _ == A b _ = a == b
instance Ord tag  => Ord  (SumWrapper tag a) where compare (A a _) (A b _) = compare a b
instance Show tag => Show (SumWrapper tag a) where show = show . display

-- | * Experimental editor, do not use yet.
editorSum
  :: (Ord tag, Show tag)
  => [(tag, Editor a)] -> (a -> tag) -> Behavior a -> UI (Editor a)
editorSum options selector ba = mdo
  let bSelected =
        let build a =
              let tag = selector a
              in A tag <$> lookup tag options
        in build <$> ba
  l <- listBox (pure $ fmap (uncurry A) options) bSelected (pure (string . show))
  let nestedEditor = factory . fromMaybe (uncurry A $ head options) <$> bSelected
  nestedElement <- sink children ((:[]) . getElement <$> nestedEditor) new
  composed <- column [element l, widget nestedElement]
  let joinE :: Event (Event a) -> UI(Event a)
      joinE = undefined -- missing in threepenny-gui
  event <- joinE (edited <$> nestedEditor <@ rumors (userSelection l))
  return $ Editor (tidings ba event) composed

instance Editable () where
  editor b = do
    t <- new
    return $ Editor (tidings b never) (getElement t)

instance a ~ Char => Editable [a] where
  editor b = do
    t <- entry b
    return $ Editor (userText t) (getElement t)

instance Editable Int where
  editor = run $ EditorFactory editor `withDefault` 0

instance Editable Double where
  editor = run $ EditorFactory editor `withDefault` 0

instance Editable Bool where
  editor b = do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ Editor (tidings b $ checkedChange t) t

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow
