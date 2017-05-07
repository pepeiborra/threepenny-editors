{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC  #-}
module Graphics.UI.Threepenny.Editors.Profunctor
  ( -- * Editors
    Base.Editor(..)
  , Base.edited
  , Base.contents
  , EditorFactory
  , createEditor
  , Editable(..)
    -- ** Editor compoosition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
    -- ** Editor constructors
  , editorUnit
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
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Editors.Base as Base

-- | A newtype wrapper that provides a 'Profunctor' instance.
newtype EditorFactory a b = EditorFactory
  { run :: Behavior a -> Compose UI Base.Editor b
  }

createEditor :: EditorFactory b a -> Behavior b -> UI (Base.Editor a)
createEditor e b = getCompose $ run e b

instance Functor (EditorFactory a) where
  fmap = dimap id

instance Profunctor EditorFactory where
  dimap g h (EditorFactory f) = EditorFactory $ \b -> h <$> f (g <$> b)

class Editable a where
  editor :: EditorFactory a a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

(|*|) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
a |*| b = EditorFactory $ \s -> run a s Base.|*| run b s

(|*) :: EditorFactory s a -> UI Element -> EditorFactory s a
a |* e = EditorFactory $ \s -> run a s Base.|* e

(*|) :: UI Element -> EditorFactory s a -> EditorFactory s a
e *| a = EditorFactory $ \s -> e Base.*| run a s

(-*-) :: EditorFactory s (b->a) -> EditorFactory s b -> EditorFactory s a
a -*- b = EditorFactory $ \s -> run a s Base.-*- run b s

(-*) :: EditorFactory s a -> UI Element -> EditorFactory s a
a -* e = EditorFactory $ \s -> run a s Base.-* e

(*-) :: UI Element -> EditorFactory s a -> EditorFactory s a
e *- a = EditorFactory $ \s -> e Base.*- run a s

editorUnit :: EditorFactory a ()
editorUnit = EditorFactory $ \_ -> Base.editor (pure ())

withDefault
  :: EditorFactory (Maybe a) (Maybe b)
  -> b
  -> EditorFactory a b
withDefault editor def = dimap Just (fromMaybe def) editor

editorReadShow :: (Read a, Show a) => EditorFactory (Maybe a) (Maybe a)
editorReadShow = EditorFactory Base.editorReadShow

editorEnumBounded
  :: (Show a, Ord a, Enum a, Bounded a)
  => Behavior (a -> UI Element) -> EditorFactory (Maybe a) (Maybe a)
editorEnumBounded display = EditorFactory $ Base.editorEnumBounded display

editorSum
  :: (Show tag, Ord tag)
  => [(tag, EditorFactory b b)] -> (b -> tag) -> EditorFactory b b
editorSum nested tagger = EditorFactory $ \b ->
  let nested' = [ (tag, run f b) | (tag, f) <- nested ]
  in Base.editorSum nested' tagger b

instance Editable () where editor = EditorFactory Base.editor
instance Editable String where editor = EditorFactory Base.editor
instance Editable Bool where editor = EditorFactory Base.editor
instance Editable (Maybe Int) where editor = EditorFactory Base.editor
instance Editable (Maybe Double) where editor = EditorFactory Base.editor

instance Editable Int where
  editor = editor `withDefault` 0

instance Editable Double where
  editor = editor `withDefault` 0

instance (Editable a, Editable b) => Editable (a,b) where
  editor = (,) <$> lmap fst editor |*| lmap snd editor
