{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# OPTIONS_GHC -Wno-duplicate-exports  #-}

module Graphics.UI.Threepenny.Editors.Types
  (
  -- * Editors
    Editor(..)
  , edited
  , contents
  , editorElement
  , EditorFactory(.., Horizontally, horizontally, Vertically, vertically)
  , dimapEF
  , lmapEF
  , applyEF
  , createEditor
  , renderEditor
  , editorFactoryElement
  , editorFactoryInput
  , editorFactoryOutput
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , fieldLayout
  , edit
  , pattern Horizontally
  , pattern Vertically
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
  , editorString
  , editorCheckBox
  , editorReadShow
  , editorEnumBounded
  , editorSelection
  , editorSum
  , editorJust
    -- ** Editor layout
  , withLayout
  , construct
  ) where

import           Control.Applicative
import           Control.Lens                          hiding (beside, children,
                                                        element, set, ( # ))
import           Data.Biapplicative
import           Data.Functor.Compose
import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Core           as UI
import           Graphics.UI.Threepenny.Editors.Layout
import           Graphics.UI.Threepenny.Editors.Utils
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

instance Bifunctor Editor where
  bimap f g (Editor t e) = Editor (g <$> t) (f e)

-- | A lens over the 'editorElement' field
editorElement :: Lens (Editor el a) (Editor el' a) el el'
editorElement f (Editor t el) = Editor t <$> f el

edited :: Editor el a -> Event a
edited = rumors . _editorTidings

contents :: Editor el a -> Behavior a
contents = facts . _editorTidings

instance Widget el => Widget (Editor el a) where
  getElement = getElement . _editorElement

renderEditor :: Renderable w => Editor w a -> UI (Editor Element a)
renderEditor = mapMOf editorElement render
  where
    mapMOf l cmd = unwrapMonad . l (WrapMonad . cmd)

-- | Create an editor to display the argument.
--   User edits are fed back via the 'edited' 'Event'.
createEditor :: Renderable w => EditorFactory a w b -> Behavior a -> UI (Editor Element b)
createEditor e b = runEF e b >>= renderEditor

-- | A function from 'Behavior' @a@ to 'Editor' @b@
--   All the three type arguments are functorial, but @a@ is contravariant.
--   'EditorFactory' is a 'Biapplicative' functor on @el@ and @b@, and
--   a 'Profunctor' on @a@ and @b@.
newtype EditorFactory a el b = EF {runEF :: Behavior a -> UI (Editor el b)}

_EditorFactory :: Iso (EditorFactory a el b) (EditorFactory a' el' b') (Behavior a -> UI (Editor el b)) (Behavior a' -> UI (Editor el' b'))
_EditorFactory = iso runEF EF

-- | A 'Setter' over the element of the editor being built
editorFactoryElement :: Setter (EditorFactory a el b) (EditorFactory a el' b) el el'
editorFactoryElement = _EditorFactory.mapped.mapped.editorElement

-- | A 'Setter' over the input thing
editorFactoryInput :: Setter (EditorFactory a el b) (EditorFactory a' el b) a' a
editorFactoryInput = _EditorFactory.argument.mapped

-- | A 'Setter' over the output thing
editorFactoryOutput :: Setter (EditorFactory a el b) (EditorFactory a el b') b b'
editorFactoryOutput = _EditorFactory.mapped.mapped.mapped

liftElement :: UI el -> EditorFactory a el ()
liftElement el = EF $ \_ -> Editor (pure ()) <$> el

bimapEF :: (el -> el') -> (b -> b') -> EditorFactory a el b -> EditorFactory a el' b'
bimapEF g h = EF . fmap (fmap (bimap g h)) . runEF

dimapEF :: (a' -> a) -> (b -> b') -> EditorFactory a el b -> EditorFactory a' el b'
dimapEF g h (EF f) = EF $ \b -> getCompose $ h <$> Compose (f (g <$> b))

-- | Applies a function over the input
lmapEF :: (a' -> a) -> EditorFactory a el b -> EditorFactory a' el b
lmapEF f = dimapEF f id

-- | Focus the editor on the field retrieved by the getter.
--   Use when composing editors via the Biapplicative interface
--
-- > personEditor :: EditorFactory Person PersonEditor Person
-- > personEditor =
-- >     bipure Person Person
-- >       <<*>> edit education editor
-- >       <<*>> edit firstName editor
-- >       <<*>> edit lastName  editor
edit :: (a' -> a) -> EditorFactory a el b -> EditorFactory a' el b
edit = lmapEF

applyEF :: (el1 -> el2 -> el) -> EditorFactory in_ el1 (a -> b) -> EditorFactory in_ el2 a -> EditorFactory in_ el b
applyEF combineElements a b = EF $ \s -> do
    a <- runEF a s
    b <- runEF b s
    return $ Editor (_editorTidings a <*> _editorTidings b) (_editorElement a `combineElements` _editorElement b)

instance Functor (EditorFactory a el) where
  fmap = dimapEF id

instance Bifunctor (EditorFactory a) where
  bimap = bimapEF

instance Biapplicative (EditorFactory a) where
  bipure w o = EF $ \_ -> return $ Editor (pure o) w
  (<<*>>) = applyEF ($)

instance Monoid el => Applicative (EditorFactory a el) where
  pure = bipure mempty
  (<*>) = applyEF mappend

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
pattern Vertically :: EditorFactory a Layout b -> EditorFactory a Vertical b
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
pattern Horizontally :: EditorFactory a Layout b -> EditorFactory a Horizontal b
pattern Horizontally {horizontally} <- (withLayout getHorizontal -> horizontally) where Horizontally a = withLayout Horizontal a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

-- | Apply a layout builder.
withLayout :: (layout -> layout') -> EditorFactory a layout b -> EditorFactory a layout' b
withLayout = over editorFactoryElement

-- | Construct a concrete 'Layout'. Useful when combining heterogeneours layout builders.
construct :: Renderable m => EditorFactory a m b -> EditorFactory a Layout b
construct = withLayout getLayout

-- | Left-right editor composition
(|*|) :: EditorFactory s Layout (b -> a) -> EditorFactory s Layout b -> EditorFactory s Layout a
a |*| b = withLayout getHorizontal $ withLayout Horizontal a <*> withLayout Horizontal b

-- | Left-right composition of an editorElement with a editor
(*|) :: UI Element -> EditorFactory s Layout a -> EditorFactory s Layout a
e *| a = withLayout getHorizontal $ liftElement(return $ horizontal e) *> withLayout Horizontal a

-- | Left-right composition of an editorElement with a editor
(|*) :: EditorFactory s Layout a -> UI Element -> EditorFactory s Layout a
a |* e = withLayout getHorizontal $ withLayout Horizontal a <* liftElement(return $ horizontal e)

-- | Left-right editor composition
(-*-) :: EditorFactory s Layout (b -> a) -> EditorFactory s Layout b -> EditorFactory s Layout a
a -*- b = withLayout getVertical $ withLayout Vertical a <*> withLayout Vertical b

-- | Left-right composition of an editorElement with a editor
(*-) :: UI Element -> EditorFactory s Layout a -> EditorFactory s Layout a
e *- a = withLayout getVertical $ liftElement(return $ vertical e) *> withLayout Vertical a

-- | Left-right composition of an editorElement with a editor
(-*) :: EditorFactory s Layout a -> UI Element -> EditorFactory s Layout a
a -* e = withLayout getVertical $ withLayout Vertical a <* liftElement(return $ vertical e)

-- | A helper that arranges a label with the field name
--   and the editor horizontally. This version takes a Layout builder as well.
fieldLayout :: (Renderable m, Renderable m') => (Layout -> m') -> String -> (out -> inn) -> EditorFactory inn m a -> EditorFactory out m' a
fieldLayout l name f e = withLayout l (string name *| first getLayout (dimapEF f id e))

-- | A helper that arranges a label with the field name
--   and the editor horizontally.
field :: Renderable m => String -> (out -> inn) -> EditorFactory inn m a -> EditorFactory out Layout a
field name f e = string name *| first getLayout (dimapEF f id e)

editorUnit :: EditorFactory b Element b
editorUnit = EF $ \b -> do
    t <- new
    return $ Editor (tidings b never) t

editorCheckBox :: EditorFactory Bool Element Bool
editorCheckBox = EF $ \b -> do
    t <- sink checked b $ input # set type_ "checkbox"
    return $ Editor (tidings b $ checkedChange t) t

editorString :: EditorFactory String TextEntry String
editorString = EF $ \b -> do
    w <- askWindow
    t <- entry b
    liftIOLater $ do
      initialValue <- currentValue b
      _ <- runUI w $ set value initialValue (element t)
      return ()
    return $ Editor (userText t) t

editorReadShow :: (Read a, Show a) => EditorFactory (Maybe a) TextEntry (Maybe a)
editorReadShow = EF $ \b -> do
    e <- runEF editorString (maybe "" show <$> b)
    let readIt "" = Nothing
        readIt x  = readMaybe x
    let t = tidings b (readIt <$> edited e)
    return $ Editor t (_editorElement e)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> EditorFactory (Maybe a) (ListBox a) (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> EditorFactory (Maybe a) (ListBox a) (Maybe a)
editorSelection options display = EF $ \b -> do
  l <- listBox options b display
  return $ Editor (tidings b (rumors $ userSelection l)) l

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: EditorFactory (Maybe b) el (Maybe b) -> EditorFactory b el b
editorJust (EF editor) = EF $ \b -> do
  e <- editor (Just <$> b)
  let ev = filterJust (edited e)
  return $ Editor (tidings b ev) (_editorElement e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag, Renderable el)
  => (Layout -> Layout -> Layout) -> [(tag, EditorFactory a el a)] -> (a -> tag) -> EditorFactory a Layout a
editorSum combineLayout options selector = EF $ \ba -> do
  options <- mapM (\(tag, EF mk) -> (tag,) <$> (mk ba >>= renderEditor)) options
  let tag = selector <$> ba
  tag' <- calmB tag
  let build a = lookup a options
  -- build a tag selector following the current tag
  l <- listBox (pure $ fmap fst options) (Just <$> tag) (pure (string . show))
  -- a placeholder for the constructor editor
  nestedEditor <-
    new # sink children ((\x -> [maybe (error "editorSum") _editorElement (build x)]) <$> tag')
  --
  let composed = combineLayout (Single (return $ getElement l)) (Single $ return nestedEditor)
  -- the result event fires when any of the nested editors or the tag selector fire.
  let editedEvents = fmap (edited . snd) options
      eTag = filterJust $ rumors (userSelection l)
      taggedOptions = sequenceA [(tag, ) <$> contents e | (tag, e) <- options]
      editedTag = filterJust $ flip lookup <$> taggedOptions <@> eTag
      editedE = head <$> unions (editedTag : editedEvents)
  return $ Editor (tidings ba editedE) composed

editorIdentity :: EditorFactory a el a -> EditorFactory (Identity a) el (Identity a)
editorIdentity = dimapEF runIdentity Identity
