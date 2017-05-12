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
  , Editable(..)
    -- ** Editor definitions
  , EditorDef
  , runEditorDef
    -- ** Editor compoosition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorReadShow
  , editorEnumBounded
  , editorSum
  , editorJust
  -- * Reexports
  , Compose(..)
  )where

import           Data.Functor.Compose
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

edited :: Editor a -> Event a
edited = rumors . editorTidings

contents :: Editor a -> Behavior a
contents = facts . editorTidings

instance Widget (Editor a) where
  getElement = editorElement

data Layout
  = Horizontal [Layout]
  | Vertical [Layout]
  | Single Element

vertical, horizontal :: Layout -> Layout -> Layout
vertical (Vertical xx) y = Vertical (xx ++ [y])
vertical x (Vertical yy) = Vertical (x:yy)
vertical x y = Vertical [x,y]

horizontal (Horizontal xx) y = Horizontal (xx ++ [y])
horizontal x (Horizontal yy) = Horizontal (x : yy)
horizontal x y = Horizontal [x,y]

single :: Element -> Layout
single = Single

runLayout :: Layout -> UI Element
runLayout (Vertical ll)   = column $ fmap runLayout ll
runLayout (Horizontal ll) = row $ fmap runLayout ll
runLayout (Single x)      = return x

data EditorDef a = EditorDef
  { editorDefTidings :: Tidings a
  , editorDefLayout  :: Layout
  }
  deriving Functor

editedDef :: EditorDef a -> Event a
editedDef = rumors . editorDefTidings

runEditorDef :: EditorDef a -> UI (Editor a)
runEditorDef def = do
  el <- runLayout (editorDefLayout def)
  return $ Editor (editorDefTidings def) el

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
  return $ EditorDef (editorDefTidings a <*> editorDefTidings b) ab

-- | Left-right composition of an element with a editor
(*|) :: UI Element -> Compose UI EditorDef a -> Compose UI EditorDef a
e *| a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (single e) (editorDefLayout a)
  return $ EditorDef (editorDefTidings a) ea

-- | Left-right composition of an element with a editor
(|*) :: Compose UI EditorDef a -> UI Element -> Compose UI EditorDef a
a |* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = horizontal (editorDefLayout a) (single e)
  return $ EditorDef (editorDefTidings a) ea

-- | Top-down editor composition
(-*-) :: Compose UI EditorDef (b -> a) -> Compose UI EditorDef b -> Compose UI EditorDef a
a -*- b = Compose $ do
  a <- getCompose a
  b <- getCompose b
  let ab = vertical (editorDefLayout a) (editorDefLayout b)
  return $ EditorDef (editorDefTidings a <*> editorDefTidings b) ab

-- | Top-down composition of an element with a editor
(*-) :: UI Element -> Compose UI EditorDef a -> Compose UI EditorDef a
e *- a = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (single e) (editorDefLayout a)
  return $ EditorDef (editorDefTidings a) ea

-- | Top-down composition of an element with a editor
(-*) :: Compose UI EditorDef a -> UI Element -> Compose UI EditorDef a
a -* e = Compose $ do
  e <- e
  a <- getCompose a
  let ea = vertical (editorDefLayout a) (single e)
  return $ EditorDef (editorDefTidings a) ea

editorReadShow :: (Read a, Show a) => Behavior (Maybe a) -> Compose UI EditorDef (Maybe a)
editorReadShow b = Compose $ do
    e <- getCompose $ editor (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> editedDef e)
    return $ EditorDef t (editorDefLayout e)

editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Behavior (Maybe a) -> Compose UI EditorDef (Maybe a)
editorEnumBounded display b = Compose $ do
  l <- listBox (pure $ enumFrom minBound) b display
  return $ EditorDef (tidings b (rumors $ userSelection l)) (single $ getElement l)


editorJust :: (Behavior (Maybe b) -> Compose UI EditorDef (Maybe b))
  -> Behavior b
  -> Compose UI EditorDef b
editorJust editor b = Compose $ do
  e <- getCompose $ editor (Just <$> b)
  let ev = filterJust (editedDef e)
  return $ EditorDef (tidings b ev) (editorDefLayout e)

data SumWrapper tag a = A {display :: tag, theEditor :: Editor a}

instance Eq  tag  => Eq   (SumWrapper tag a) where A a _ == A b _ = a == b
instance Ord tag  => Ord  (SumWrapper tag a) where compare (A a _) (A b _) = compare a b
instance Show tag => Show (SumWrapper tag a) where show = show . display

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => [(tag, Compose UI EditorDef a)] -> (a -> tag) -> Behavior a -> Compose UI EditorDef a
editorSum options selector ba = Compose $ do
  w <- askWindow
  options <- traverse (\(tag, Compose mk) -> (tag,) <$> (mk >>= runEditorDef)) options
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
  nestedEditorDef <- new
  -- when the user selects a tag, refresh the nested editor
  _ <-
    liftIO $
    register (filterJust $ rumors (userSelection l)) $ \x ->
      runUI w $ set' children [getElement $ theEditor x] nestedEditorDef
  --
  let composed = Vertical [single (getElement l), single nestedEditorDef]
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ fmap display <$> rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ EditorDef (tidings ba editedE) composed

instance Editable () where
  editor b = Compose $ do
    t <- new
    return $ EditorDef (tidings b never) (single t)

instance a ~ Char => Editable [a] where
  editor b = Compose $ do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ EditorDef (userText t) (single $ getElement t)

instance Editable Bool where
  editor b = Compose $ do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ EditorDef (tidings b $ checkedChange t) (single t)

instance Editable (Maybe Int) where editor = editorReadShow
instance Editable (Maybe Double) where editor = editorReadShow
instance Editable Int where editor = editorJust editor
instance Editable Double where editor = editorJust editor


instance (Editable a, Editable b) => Editable (a,b) where
  editor b = (,) <$> editor (fst <$> b) |*| editor (snd <$> b)
