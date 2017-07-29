{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Graphics.UI.Threepenny.Editors.Types
  (
  -- * Editors
    Editor(..)
  , edited
  , contents
  , editorElement
  , editorTidings
  , EditorFactory(..)
  , createEditor
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , Horizontally(..)
  , Vertically(..)
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
  ) where

import           Control.Applicative
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Profunctor
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Utils
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Widgets

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

-- | Create an editor to display the argument.
--   User edits are fed back via the 'edited' 'Event'.
createEditor :: EditorFactory Layout b a -> Behavior b -> UI (Editor Element a)
createEditor e b = runEF e b >>= runEditor

-- | A function from 'Behavior' @a@ to 'Editor' @b@
newtype EditorFactory el a b = EF {runEF :: Behavior a -> UI (Editor el b)}

instance Functor (EditorFactory el a) where
  fmap = dimap id

instance Profunctor (EditorFactory el) where
  dimap g h (EF f) = EF $ \b -> getCompose $ h <$> Compose (f (g <$> b))

-- | Applicative instance for vertical composition of editor factories.
--
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = vertically $ do
-- >       firstName <- Vertically $ field "First:" firstName editor
-- >       lastName  <- Vertically $ field "Last:"  lastName editor
-- >       age       <- Vertically $ field "Age:"   age editor
-- >       return Person{..}

newtype Vertically a b = Vertically {vertically :: EditorFactory Layout a b} deriving (Functor, Profunctor)
instance Applicative (Vertically a) where
  pure x = Vertically $ const x <$> editorUnit
  Vertically a <*> Vertically b = Vertically (a -*- b)

-- | Applicative instance for horizontal composition of editor factories.
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = horizontally $ do
-- >       firstName <- Horizontally $ field "First:" firstName editor
-- >       lastName  <- Horizontally $ field "Last:"  lastName editor
-- >       age       <- Horizontally $ field "Age:"   age editor
-- >       return Person{..}
newtype Horizontally a b = Horizontally {horizontally :: EditorFactory Layout a b} deriving (Functor, Profunctor)
instance Applicative (Horizontally a) where
  pure x = Horizontally $ const x <$> editorUnit
  Horizontally a <*> Horizontally b = Horizontally (a |*| b)


infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Left-right editor composition
(|*|) :: EditorFactory Layout s (b -> a) -> EditorFactory Layout s b -> EditorFactory Layout s a
a |*| b = EF $ \s -> do
  a <- runEF a s
  b <- runEF b s
  let ab = horizontal (_editorElement a) (_editorElement b)
  return $ Editor (_editorTidings a <*> _editorTidings b) ab

-- | Left-right composition of an editorElement with a editor
(*|) :: UI Element -> EditorFactory Layout s a -> EditorFactory Layout s a
e *| a = EF $ \s -> do
  e <- e
  a <- runEF a s
  let ea = horizontal (Single e) (_editorElement a)
  return $ Editor (_editorTidings a) ea

-- | Left-right composition of an editorElement with a editor
(|*) :: EditorFactory Layout s a -> UI Element -> EditorFactory Layout s a
a |* e = EF $ \s -> do
  e <- e
  a <- runEF a s
  let ea = horizontal (_editorElement a) (Single e)
  return $ Editor (_editorTidings a) ea

-- | Top-down editor composition
(-*-) :: EditorFactory Layout s (b -> a) -> EditorFactory Layout s b -> EditorFactory Layout s a
a -*- b = EF $ \s -> do
  a <- runEF a s
  b <- runEF b s
  let ab = vertical (_editorElement a) (_editorElement b)
  return $ Editor (_editorTidings a <*> _editorTidings b) ab

-- | Top-down composition of an editorElement with a editor
(*-) :: UI Element -> EditorFactory Layout s a -> EditorFactory Layout s a
e *- a = EF $ \s -> do
  e <- e
  a <- runEF a s
  let ea = vertical (Single e) (_editorElement a)
  return $ Editor (_editorTidings a) ea

-- | Top-down composition of an editorElement with a editor
(-*) :: EditorFactory Layout s a -> UI Element -> EditorFactory Layout s a
a -* e = EF $ \s -> do
  e <- e
  a <- runEF a s
  let ea = vertical (_editorElement a) (Single e)
  return $ Editor (_editorTidings a) ea

-- | A helper that arranges a label with the field name
--   and the editor horizontally.
field :: String -> (out -> inn) -> EditorFactory Layout inn a -> EditorFactory Layout out a
field name f e = string name *| lmap f e

editorUnit :: EditorFactory Layout b b
editorUnit = EF $ \b -> do
    t <- new
    return $ Editor (tidings b never) (Single t)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> EditorFactory Layout (Maybe a) (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> EditorFactory Layout (Maybe a) (Maybe a)
editorSelection options display = EF $ \b -> do
  l <- listBox options b display
  return $ Editor (tidings b (rumors $ userSelection l)) (Single $ getElement l)

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: EditorFactory el (Maybe b) (Maybe b) -> EditorFactory el b b
editorJust (EF editor) = EF $ \b -> do
  e <- editor (Just <$> b)
  let ev = filterJust (edited e)
  return $ Editor (tidings b ev) (_editorElement e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => (Layout -> Layout -> Layout) -> [(tag, EditorFactory Layout a a)] -> (a -> tag) -> EditorFactory Layout a a
editorSum combineLayout options selector = EF $ \ba -> do
  options <- mapM (\(tag, EF mk) -> (tag,) <$> (mk ba >>= runEditor)) options
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

editorIdentity :: EditorFactory el a a -> EditorFactory el (Identity a) (Identity a)
editorIdentity = dimap runIdentity Identity
