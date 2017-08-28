{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-duplicate-exports  #-}

module Graphics.UI.Threepenny.Editors.Types
  (
  -- * GenericWidgets
    GenericWidget(..)
  , edited
  , contents
  , widgetControl
  , widgetTidings
  , liftElement
  , Editor(.., Horizontally, horizontally, Vertically, vertically)
  , dimapE
  , lmapE
  , applyE
    -- ** GenericWidget composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , fieldLayout
  , edit
  , pattern Horizontally
  , pattern Vertically
    -- ** GenericWidget constructors
  , editorUnit
  , editorIdentity
  , editorString
  , editorCheckBox
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
    -- ** GenericWidget layout
  , withLayout
  , construct
  ) where

import           Data.Biapplicative
import           Data.Coerce
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Profunctor
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Utils
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.Widgets
import           Text.Read

-- | A widget for editing values of type @a@.
data GenericWidget control a = GenericWidget
  { widgetTidings :: Tidings a
  , widgetControl :: control
  }
  deriving Functor

instance Bifunctor GenericWidget where
  bimap f g (GenericWidget t e) = GenericWidget (g <$> t) (f e)

traverseControl :: Applicative f => (control -> f control') -> GenericWidget control a -> f (GenericWidget control' a)
traverseControl f (GenericWidget t e) = GenericWidget t <$> f e

edited :: GenericWidget el a -> Event a
edited = rumors . widgetTidings

contents :: GenericWidget el a -> Behavior a
contents = facts . widgetTidings

instance Widget el => Widget (GenericWidget el a) where
  getElement = getElement . widgetControl

instance Renderable el => Renderable (GenericWidget el a) where
  render = render . widgetControl

renderEditor :: Renderable w => GenericWidget w a -> UI (GenericWidget Element a)
renderEditor = traverseControl render

-- | A widget @el@ for editing @b@ values while displaying @a@ values.
--   For obvious reasons, @a@ and @b@ are usually the same type, except while composing editors.
--   All the three type arguments are functorial, but @a@ is contravariant.
--   'Editor' is a 'Biapplicative' functor on @el@ and @b@, and a 'Profunctor' on @a@ and @b@.
--
--   Editors compose using the Applicative interface when @el@ is monoidal
--   or more generally with the Biapplicative interface. In both cases the
--   Profunctor 'lmap' is used to select the value to display.
--
--   For an example of the Applicative interface, let's assemble the editor for a tuple of values:
--
--   > editorTuple = (,) <$> lmap fst editable <*> lmap snd editable

newtype Editor a el b = Editor {
  -- | Create an editor to display the argument.
  --   User edits are fed back via the 'edited' 'Event'.
  create :: Behavior a -> UI (GenericWidget el b)
  }

-- | Lift an HTML element into a vacuous editor.
liftElement :: UI el -> Editor a el ()
liftElement el = Editor $ \_ -> GenericWidget (pure ()) <$> el

bimapEditor :: (el -> el') -> (b -> b') -> Editor a el b -> Editor a el' b'
bimapEditor g h = Editor . fmap (fmap (bimap g h)) . create

dimapE :: (a' -> a) -> (b -> b') -> Editor a el b -> Editor a' el b'
dimapE g h (Editor f) = Editor $ dimap (fmap g) (fmapUIGW h) f
  where
    fmapUIGW = coerce (fmap @ (Compose UI _))

-- | Applies a function over the input
lmapE :: (a' -> a) -> Editor a el b -> Editor a' el b
lmapE f = dimapE f id

-- | Use when composing Biapplicative editors to focus on a field.
--
-- > personEditor :: Editor Person PersonEditor Person
-- > personEditor =
-- >     bipure Person Person
-- >       <<*>> edit education editor
-- >       <<*>> edit firstName editor
-- >       <<*>> edit lastName  editor
edit :: (a' -> a) -> Editor a el b -> Editor a' el b
edit = lmapE

applyE :: (el1 -> el2 -> el) -> Editor in_ el1 (a -> b) -> Editor in_ el2 a -> Editor in_ el b
applyE combineElements a b = Editor $ \s -> do
    a <- create a s
    b <- create b s
    return $ GenericWidget (widgetTidings a <*> widgetTidings b) (widgetControl a `combineElements` widgetControl b)

instance Functor (Editor a el) where
  fmap = dimapE id

instance Bifunctor (Editor a) where
  bimap = bimapEditor

instance Biapplicative (Editor a) where
  bipure w o = Editor $ \_ -> return $ GenericWidget (pure o) w
  (<<*>>) = applyE ($)

instance Monoid el => Applicative (Editor a el) where
  pure = bipure mempty
  (<*>) = applyE mappend

-- | Applicative modifier for vertical composition of editor factories.
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = vertically $ do
-- >       firstName <- Vertically $ field "First:" firstName editor
-- >       lastName  <- Vertically $ field "Last:"  lastName editor
-- >       age       <- Vertically $ field "Age:"   age editor
-- >       return Person{..}
--
-- DEPRECATED: Use the 'Vertical' layout builder instead
pattern Vertically :: Editor a Layout b -> Editor a Vertical b
pattern Vertically {vertically} <- (withLayout getVertical -> vertically) where Vertically a = withLayout Vertical a

-- | Applicative modifier for horizontal composition of editor factories.
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = horizontally $ do
-- >       firstName <- Horizontally $ field "First:" firstName editor
-- >       lastName  <- Horizontally $ field "Last:"  lastName editor
-- >       age       <- Horizontally $ field "Age:"   age editor
-- >       return Person{..}
--
-- DEPRECATED: Use the 'Horizontal' layout builder instead
pattern Horizontally :: Editor a Layout b -> Editor a Horizontal b
pattern Horizontally {horizontally} <- (withLayout getHorizontal -> horizontally) where Horizontally a = withLayout Horizontal a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Apply a layout builder.
withLayout :: (layout -> layout') -> Editor a layout b -> Editor a layout' b
withLayout f = bimap f id

-- | Construct a concrete 'Layout'. Useful when combining heterogeneours layout builders.
construct :: Renderable m => Editor a m b -> Editor a Layout b
construct = withLayout getLayout

-- | Left-right editor composition
(|*|) :: Editor s Layout (b -> a) -> Editor s Layout b -> Editor s Layout a
a |*| b = withLayout getHorizontal $ withLayout Horizontal a <*> withLayout Horizontal b

-- | Left-right composition of an element with a editor
(*|) :: UI Element -> Editor s Layout a -> Editor s Layout a
e *| a = withLayout getHorizontal $ liftElement(return $ horizontal e) *> withLayout Horizontal a

-- | Left-right composition of an element with a editor
(|*) :: Editor s Layout a -> UI Element -> Editor s Layout a
a |* e = withLayout getHorizontal $ withLayout Horizontal a <* liftElement(return $ horizontal e)

-- | Left-right editor composition
(-*-) :: Editor s Layout (b -> a) -> Editor s Layout b -> Editor s Layout a
a -*- b = withLayout getVertical $ withLayout Vertical a <*> withLayout Vertical b

-- | Left-right composition of an element with a editor
(*-) :: UI Element -> Editor s Layout a -> Editor s Layout a
e *- a = withLayout getVertical $ liftElement(return $ vertical e) *> withLayout Vertical a

-- | Left-right composition of an element with a editor
(-*) :: Editor s Layout a -> UI Element -> Editor s Layout a
a -* e = withLayout getVertical $ withLayout Vertical a <* liftElement(return $ vertical e)

-- | A helper that arranges a label with the field name
--   and the editor horizontally. This version takes a Layout builder as well.
fieldLayout :: (Renderable m, Renderable m') => (Layout -> m') -> String -> (out -> inn) -> Editor inn m a -> Editor out m' a
fieldLayout l name f e = withLayout l (string name *| first getLayout (dimapE f id e))

-- | A helper that arranges a label with the field name
--   and the editor horizontally.
field :: Renderable m => String -> (out -> inn) -> Editor inn m a -> Editor out Layout a
field name f e = string name *| first getLayout (dimapE f id e)

editorUnit :: Editor b Element b
editorUnit = Editor $ \b -> do
    t <- new
    return $ GenericWidget (tidings b never) t

editorCheckBox :: Editor Bool Element Bool
editorCheckBox = Editor $ \b -> do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ GenericWidget (tidings b $ checkedChange t) t

editorString :: Editor String TextEntry String
editorString = Editor $ \b -> do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ GenericWidget (userText t) t

editorReadShow :: (Read a, Show a) => Editor (Maybe a) TextEntry (Maybe a)
editorReadShow = Editor $ \b -> do
    e <- create editorString (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> edited e)
    return $ GenericWidget t (widgetControl e)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> Editor (Maybe a) (ListBox a) (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> Editor (Maybe a) (ListBox a) (Maybe a)
editorSelection options display = Editor $ \b -> do
  l <- listBox options b display
  return $ GenericWidget (tidings b (rumors $ userSelection l)) l

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: Editor (Maybe b) el (Maybe b) -> Editor b el b
editorJust (Editor editor) = Editor $ \b -> do
  e <- editor (Just <$> b)
  let ev = filterJust (edited e)
  return $ GenericWidget (tidings b ev) (widgetControl e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag, Renderable el)
  => (Layout -> Layout -> Layout) -> [(tag, Editor a el a)] -> (a -> tag) -> Editor a Layout a
editorSum combineLayout options selector = Editor $ \ba -> do
  options <- mapM (\(tag, Editor mk) -> (tag,) <$> (mk ba >>= renderEditor)) options
  let tag = selector <$> ba
  tag' <- calmB tag
  let build a = lookup a options
  -- build a tag selector following the current tag
  l <- listBox (pure $ fmap fst options) (Just <$> tag) (pure (string . show))
  -- a placeholder for the constructor editor
  nestedEditor <-
    new # sink children ((\x -> [maybe (error "editorSum") widgetControl (build x)]) <$> tag')
  --
  let composed = combineLayout (Single (return $ getElement l)) (Single $ return nestedEditor)
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ GenericWidget (tidings ba editedE) composed

editorIdentity :: Editor a el a -> Editor (Identity a) el (Identity a)
editorIdentity = dimapE runIdentity Identity
