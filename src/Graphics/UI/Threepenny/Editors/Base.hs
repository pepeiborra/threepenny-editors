{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC  #-}
module Graphics.UI.Threepenny.Editors.Base
  ( -- * Editors
    Editor(..)
  , edited
  , contents
  , Editable(..)
    -- ** Editor definitions
  , EditorDef(..)
  , runEditorDef
  , liftEditorDef
    -- ** Layouts
  , Layout(..)
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

import           Data.Foldable (length)
import           Data.Functor.Compose
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Exts (IsList(..))
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core as UI
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

edited :: Editor a -> Event a
edited = rumors . editorTidings

contents :: Editor a -> Behavior a
contents = facts . editorTidings

instance Widget (Editor a) where
  getElement = editorElement

newtype Layout
  = Grid (Seq (Seq (Maybe Element)))-- ^ A non empty list of rows, where all the rows are assumed to have the same length

vertical, horizontal :: Layout -> Layout -> Layout
vertical (Grid rows@(length.head.toList -> l1)) (Grid rows'@(length.head.toList -> l2)) =
    Grid $ fmap pad1 rows <> fmap pad2 rows'
  where
    pad l1 l2 | l1 >= l2   = id
              | otherwise = (<> Seq.replicate (l2-l1) Nothing)
    pad1 = pad l1 l2
    pad2 = pad l2 l1

horizontal (Grid rows@(length.head.toList -> l1)) (Grid rows'@(length.head.toList -> l2)) =
    Grid $ Seq.zipWith (<>) (pad1 rows) (pad2 rows')
  where
    pad l1 l2 | l1 >= l2 = id
              | otherwise = \x -> let padding = Seq.replicate (length $ head $ toList x) Nothing in x <> Seq.replicate (l2-l1) padding
    pad1 = pad l1 l2
    pad2 = pad l2 l1

single :: Element -> Layout
single x = Grid [[Just x]]

runLayout :: Layout -> UI Element
runLayout (Grid rows) = grid (toList $ fmap (fmap (maybe new return). toList) rows)

data EditorDef a = EditorDef
  { editorDefTidings :: Tidings a
  , editorDefLayout  :: Layout
  , editorDefSetup   :: UI Element -> UI Element
  }
  deriving Functor

editorDef :: Tidings a -> Layout -> EditorDef a
editorDef tidings layout = EditorDef tidings layout id

editedDef :: EditorDef a -> Event a
editedDef = rumors . editorDefTidings

runEditorDef :: EditorDef a -> UI (Editor a)
runEditorDef def = do
  el <- runLayout (editorDefLayout def) # editorDefSetup def
  return $ Editor (editorDefTidings def) el

liftEditorDef :: (UI Element -> UI Element) -> EditorDef a -> EditorDef a
liftEditorDef f def = def{editorDefSetup = f . editorDefSetup def }

-- | The class of Editable datatypes.
class Editable a where
  -- | The editor factory
  editor :: Behavior a -> Compose UI EditorDef a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Left-right editor composition
(|*|) :: Compose UI EditorDef (b -> a) -> Compose UI EditorDef b -> Compose UI EditorDef a
a |*| b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  let ab = horizontal (editorDefLayout a) (editorDefLayout b)
  return $ editorDef (editorDefTidings a <*> editorDefTidings b) ab

-- | Left-right composition of an element with a editor
(*|) :: UI Element -> Compose UI EditorDef a -> Compose UI EditorDef a
e *| a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (single e) (editorDefLayout a)
  return $ editorDef (editorDefTidings a) ea

-- | Left-right composition of an element with a editor
(|*) :: Compose UI EditorDef a -> UI Element -> Compose UI EditorDef a
a |* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (editorDefLayout a) (single e)
  return $ editorDef (editorDefTidings a) ea

-- | Top-down editor composition
(-*-) :: Compose UI EditorDef (b -> a) -> Compose UI EditorDef b -> Compose UI EditorDef a
a -*- b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  let ab = vertical (editorDefLayout a) (editorDefLayout b)
  return $ editorDef (editorDefTidings a <*> editorDefTidings b) ab

-- | Top-down composition of an element with a editor
(*-) :: UI Element -> Compose UI EditorDef a -> Compose UI EditorDef a
e *- a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (single e) (editorDefLayout a)
  return $ editorDef (editorDefTidings a) ea

-- | Top-down composition of an element with a editor
(-*) :: Compose UI EditorDef a -> UI Element -> Compose UI EditorDef a
a -* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (editorDefLayout a) (single e)
  return $ editorDef (editorDefTidings a) ea

editorReadShow :: (Read a, Show a) => Behavior (Maybe a) -> Compose UI EditorDef (Maybe a)
editorReadShow b = Compose $ do
    e <- getCompose $ editor (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> editedDef e)
    return $ editorDef t (editorDefLayout e)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI EditorDef (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI EditorDef (Maybe a)
editorSelection options display b = Compose $ do
  l <- listBox options b display
  return $ editorDef (tidings b (rumors $ userSelection l)) (single $ getElement l)

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: (Behavior (Maybe b) -> Compose UI EditorDef (Maybe b))
  -> Behavior b
  -> Compose UI EditorDef b
editorJust editor b = Compose $ do
  e <- getCompose $ editor (Just <$> b)
  let ev = filterJust (editedDef e)
  return $ editorDef (tidings b ev) (editorDefLayout e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => [(tag, Compose UI EditorDef a)] -> (a -> tag) -> Behavior a -> Compose UI EditorDef a
editorSum options selector ba = Compose $ do
  options <- mapM (\(tag, Compose mk) -> (tag,) <$> (mk >>= runEditorDef)) options
  let tag = selector <$> ba
  tag' <- calmB tag
  let build a = lookup a options
  -- build a tag selector following the current tag
  l <- listBox (pure $ fmap fst options) (Just <$> tag) (pure (string . show))
  -- a placeholder for the constructor editor
  nestedEditorDef <-
    new # sink children ((\x -> [maybe (error "editorSum") editorElement (build x)]) <$> tag')
  --
  let composed = vertical (single (getElement l)) (single nestedEditorDef)
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ editorDef (tidings ba editedE) composed

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
    return $ editorDef (tidings b never) (single t)

instance a ~ Char => Editable [a] where
  editor b = Compose $ do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ editorDef (userText t) (single $ getElement t)

instance Editable Bool where
  editor b = Compose $ do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ editorDef (tidings b $ checkedChange t) (single t)

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow
instance Editable Int where editor = editorJust editor
instance Editable Double where editor = editorJust editor


instance (Editable a, Editable b) => Editable (a,b) where
  editor b = (,) <$> editor (fst <$> b) |*| editor (snd <$> b)
