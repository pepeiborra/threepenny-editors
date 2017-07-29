{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC  #-}
module Graphics.UI.Threepenny.Editors.Base
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , editorElement
  , editorTidings
  , Editable(..)
  , runEditor
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
  -- * Reexports
  , Compose(..)
  )where

import           Control.Applicative
import           Data.Functor.Compose
import           Data.Maybe
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core as UI
import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.Widgets
import           Text.Read

-- | A widget for editing values of type @a@.
data Editor editorElement a = Editor
  { _editorTidings :: Tidings a
  , _editorElement :: editorElement
  }
  deriving Functor

-- | A lens over the 'editorElement' field
editorElement :: Functor f => (el -> f el') -> Editor el a -> f (Editor el' a)
editorElement f (Editor t el) = Editor t <$> f el

-- | A lens over the 'editorTidings' field
editorTidings :: Functor f => (el -> f el') -> Editor el a -> f (Editor el' a)
editorTidings f (Editor t el) = Editor t <$> f el

edited :: Editor el a -> Event a
edited = rumors . _editorTidings

contents :: Editor el a -> Behavior a
contents = facts . _editorTidings

instance Widget el => Widget (Editor el a) where
  getElement = getElement . _editorElement

runEditor :: Editor Layout a -> UI (Editor Element a)
runEditor = mapMOf editorElement runLayout
  where
    mapMOf l cmd = unwrapMonad . l (WrapMonad . cmd)

-- | The class of Editable datatypes.
class Editable a where
  -- | The editor factory
  editor :: Behavior a -> Compose UI (Editor Layout) a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Left-right editor composition
(|*|) :: Compose UI (Editor Layout) (b -> a) -> Compose UI (Editor Layout) b -> Compose UI (Editor Layout) a
a |*| b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  let ab = horizontal (_editorElement a) (_editorElement b)
  return $ Editor (_editorTidings a <*> _editorTidings b) ab

-- | Left-right composition of an editorElement with a editor
(*|) :: UI Element -> Compose UI (Editor Layout) a -> Compose UI (Editor Layout) a
e *| a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (Single e) (_editorElement a)
  return $ Editor (_editorTidings a) ea

-- | Left-right composition of an editorElement with a editor
(|*) :: Compose UI (Editor Layout) a -> UI Element -> Compose UI (Editor Layout) a
a |* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (_editorElement a) (Single e)
  return $ Editor (_editorTidings a) ea

-- | Top-down editor composition
(-*-) :: Compose UI (Editor Layout) (b -> a) -> Compose UI (Editor Layout) b -> Compose UI (Editor Layout) a
a -*- b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  let ab = vertical (_editorElement a) (_editorElement b)
  return $ Editor (_editorTidings a <*> _editorTidings b) ab

-- | Top-down composition of an editorElement with a editor
(*-) :: UI Element -> Compose UI (Editor Layout) a -> Compose UI (Editor Layout) a
e *- a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (Single e) (_editorElement a)
  return $ Editor (_editorTidings a) ea

-- | Top-down composition of an editorElement with a editor
(-*) :: Compose UI (Editor Layout) a -> UI Element -> Compose UI (Editor Layout) a
a -* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (_editorElement a) (Single e)
  return $ Editor (_editorTidings a) ea

editorReadShow :: (Read a, Show a) => Behavior (Maybe a) -> Compose UI (Editor Layout) (Maybe a)
editorReadShow b = Compose $ do
    e <- getCompose $ editor (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> edited e)
    return $ Editor t (_editorElement e)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI (Editor Layout) (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI (Editor Layout) (Maybe a)
editorSelection options display b = Compose $ do
  l <- listBox options b display
  return $ Editor (tidings b (rumors $ userSelection l)) (Single $ getElement l)

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: (Behavior (Maybe b) -> Compose UI (Editor Layout) (Maybe b))
  -> Behavior b
  -> Compose UI (Editor Layout) b
editorJust editor b = Compose $ do
  e <- getCompose $ editor (Just <$> b)
  let ev = filterJust (edited e)
  return $ Editor (tidings b ev) (_editorElement e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => (Layout -> Layout -> Layout) -> [(tag, Compose UI (Editor Layout) a)] -> (a -> tag) -> Behavior a -> Compose UI (Editor Layout) a
editorSum combineLayout options selector ba = Compose $ do
  options <- mapM (\(tag, Compose mk) -> (tag,) <$> (mk >>= runEditor)) options
  let tag = selector <$> ba
  tag' <- calmB tag
  let build a = lookup a options
  -- build a tag selector following the current tag
  l <- listBox (pure $ fmap fst options) (Just <$> tag) (pure (string . show))
  -- a placeholder for the constructor editor
  nestedEditor <-
    new # sink children ((\x -> [maybe (error "editorSum") _editorElement (build x)]) <$> tag')
  --
  let composed = combineLayout (Single (getElement l)) (Single nestedEditor)
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ Editor (tidings ba editedE) composed

-- | Returns a new behavior that only notifies for new values.
calmB :: Eq a => Behavior a -> UI (Behavior a)
calmB b = do
  w <- askWindow
  (e, trigger) <- liftIO newEvent
  liftIOLater $ do
    current <- currentValue b
    trigger current
    runUI w $ onChanges b (liftIO . trigger)
  eCalm <- calmE e
  fmap (fromMaybe (error "calmB")) <$> stepper Nothing (Just <$> eCalm)

data Memory a = Empty | New a | Same a
updateMemory :: Eq a => a -> Memory a -> Memory a
updateMemory x Empty  = New x
updateMemory x (New  a) | a /= x = New x
updateMemory x (Same a) | a /= x = New x
updateMemory x _ = Same x
isNew :: Memory a -> Maybe a
isNew (New x) = Just x
isNew _ = Nothing

-- | Returns a new 'Event' that skips consecutive triggers with the same value.
calmE :: Eq a => Event a -> UI (Event a)
calmE e =
  filterJust . fmap isNew <$> accumE Empty (updateMemory <$> e)

instance Editable () where
  editor b = Compose $ do
    t <- new
    return $ Editor (tidings b never) (Single t)

instance a ~ Char => Editable [a] where
  editor b = Compose $ do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ Editor (userText t) (Single $ getElement t)

instance Editable Bool where
  editor b = Compose $ do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ Editor (tidings b $ checkedChange t) (Single t)

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow
instance Editable Int where editor = editorJust editor
instance Editable Double where editor = editorJust editor


instance (Editable a, Editable b) => Editable (a,b) where
  editor b = (,) <$> editor (fst <$> b) |*| editor (snd <$> b)
