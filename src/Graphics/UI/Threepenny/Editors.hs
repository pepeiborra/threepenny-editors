{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC  #-}
module Graphics.UI.Threepenny.Editors
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , Editable(..)
    -- ** Editor compoosition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorReadShow
  , editorEnumBounded
  , editorSum
  , withDefault
  -- * Reexports
  , Compose(..)
  )where

import           Data.Functor.Compose
import           Data.Maybe
import           Data.Profunctor
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.Widgets
import           Text.Read

-- | A widget for editing values of type @a@.
data Editor a = Editor
  { editorTidings :: Tidings a
  , editorElement :: Element
  }
  deriving Functor

instance Widget (Editor a) where
  getElement = editorElement

-- | A newtype wrapper that provides a 'Profunctor' instance.
newtype EditorFactory a b = EditorFactory { run :: Behavior a -> Compose UI Editor b }

instance Profunctor EditorFactory where
  dimap g h (EditorFactory f) = EditorFactory $ \b -> h <$> f (g <$> b)

-- | The class of Editable datatypes.
class Editable a where
  -- | The editor factory
  editor :: Behavior a -> Compose UI Editor a

edited :: Editor a -> Event a
edited = rumors . editorTidings

contents :: Editor a -> Behavior a
contents = facts . editorTidings

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Left-right editor composition
(|*|) :: Compose UI Editor (b -> a) -> Compose UI Editor b -> Compose UI Editor a
a |*| b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  ab <- row [return $ getElement a, return $ getElement b]
  return $ Editor (editorTidings a <*> editorTidings b) ab

-- | Left-right composition of an element with a editor
(*|) :: UI Element -> Compose UI Editor a -> Compose UI Editor a
e *| a = Compose $ do
  e <- e
  a <- getCompose a
  ea <- row [return e, return $ getElement a]
  return $ Editor (editorTidings a) ea

-- | Left-right composition of an element with a editor
(|*) :: Compose UI Editor a -> UI Element -> Compose UI Editor a
a |* e = Compose $ do
  e <- e
  a <- getCompose a
  ea <- row [return $ getElement a, return e]
  return $ Editor (editorTidings a) ea

-- | Top-down editor composition
(-*-) :: Compose UI Editor (b -> a) -> Compose UI Editor b -> Compose UI Editor a
a -*- b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  ab <- column [return $ getElement a, return $ getElement b]
  return $ Editor (editorTidings a <*> editorTidings b) ab

-- | Top-down composition of an element with a editor
(*-) :: UI Element -> Compose UI Editor a -> Compose UI Editor a
e *- a = Compose $ do
  e <- e
  a <- getCompose a
  ea <- column [return e, return $ getElement a]
  return $ Editor (editorTidings a) ea

-- | Top-down composition of an element with a editor
(-*) :: Compose UI Editor a -> UI Element -> Compose UI Editor a
a -* e = Compose $ do
  e <- e
  a <- getCompose a
  ea <- column [return $ getElement a, return e]
  return $ Editor (editorTidings a) ea

editorReadShow :: (Read a, Show a) => Behavior (Maybe a) -> Compose UI Editor (Maybe a)
editorReadShow b = Compose $ do
    e <- getCompose $ editor (show <$> b)
    let t = tidings b (filterJust $ readMaybe <$> edited e)
    return $ Editor t (getElement e)

editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI Editor (Maybe a)
editorEnumBounded display b = Compose $ do
  l <- listBox (pure $ enumFrom minBound) b display
  return $ Editor (userSelection l) (getElement l)

withDefault
  :: EditorFactory (Maybe a) (Maybe b)
  -> b
  -> EditorFactory a b
withDefault editor def = dimap Just (fromMaybe def) editor

data SumWrapper tag a = A {display :: tag, theEditor :: Editor a}

instance Eq  tag  => Eq   (SumWrapper tag a) where A a _ == A b _ = a == b
instance Ord tag  => Ord  (SumWrapper tag a) where compare (A a _) (A b _) = compare a b
instance Show tag => Show (SumWrapper tag a) where show = show . display

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => [(tag, Compose UI Editor a)] -> (a -> tag) -> Behavior a -> Compose UI Editor a
editorSum options selector ba = Compose $ do
  w <- askWindow
  options <- traverse (\(tag, Compose mk) -> (tag,) <$> mk) options
  -- extract the tag from the current value
  let bSelected =
        let build a =
              let tag = selector a
              in A tag <$> lookup tag options
        in build <$> ba
  -- build a tag selector following the current tag
  l <-
    listBox (pure $ fmap (uncurry A) options) bSelected (pure (string . show))
  -- a placeholder for the constructor editor
  nestedEditor <- new
  -- when the user selects a tag, refresh the nested editor
  _ <-
    liftIO $
    register (filterJust $ rumors (userSelection l)) $ \x ->
      runUI w $ set' children [getElement $ theEditor x] nestedEditor
  --
  composed <- column [element l, widget nestedEditor]
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ fmap display <$> rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ Editor (tidings ba editedE) composed

instance Editable () where
  editor b = Compose $ do
    t <- new
    return $ Editor (tidings b never) (getElement t)

instance a ~ Char => Editable [a] where
  editor b = Compose $ do
    t <- entry b
    return $ Editor (userText t) (getElement t)

instance Editable Int where
  editor = run $ EditorFactory editor `withDefault` 0

instance Editable Double where
  editor = run $ EditorFactory editor `withDefault` 0

instance Editable Bool where
  editor b = Compose $ do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ Editor (tidings b $ checkedChange t) t

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow

instance (Editable a, Editable b) => Editable (a,b) where
  editor b = (,) <$> editor (fst <$> b) |*| editor (snd <$> b)
