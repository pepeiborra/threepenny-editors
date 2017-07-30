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
  , createEditor
  , layoutEditor
  , editorFactoryElement
  , editorFactoryInput
  , editorFactoryOutput
    -- ** Editor composition
  , (|*|), (|*), (*|)
  , (-*-), (-*), (*-)
  , field
  , fieldLayout
  , pattern Horizontally
  , pattern Vertically
    -- ** Editor constructors
  , editorUnit
  , editorIdentity
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
                                                        ( # ))
import           Data.Functor.Compose
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

layoutEditor :: Editor Layout a -> UI (Editor Element a)
layoutEditor = mapMOf editorElement runLayout
  where
    mapMOf l cmd = unwrapMonad . l (WrapMonad . cmd)

-- | Create an editor to display the argument.
--   User edits are fed back via the 'edited' 'Event'.
createEditor :: EditorFactory a Layout b -> Behavior a -> UI (Editor Element b)
createEditor e b = runEF e b >>= layoutEditor

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

lmapEF :: (a' -> a) -> EditorFactory a el b -> EditorFactory a' el b
lmapEF f = dimapEF f id

instance Functor (EditorFactory a el) where
  fmap = dimapEF id

instance Bifunctor (EditorFactory a) where
  bimap = bimapEF

-- instance Profunctor (EditorFactory el) where
--  dimap = dimapEF

instance Monoid el => Applicative (EditorFactory a el) where
  pure x = EF $ \_ -> return $ Editor (pure x) mempty
  a <*> b = EF $ \s -> do
    a <- runEF a s
    b <- runEF b s
    let ab = mappend (_editorElement a) (_editorElement b)
    return $ Editor (_editorTidings a <*> _editorTidings b) ab

-- | Applicative modifier for vertical composition of editor factories.
--
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = vertically $ do
-- >       firstName <- Vertically $ field "First:" firstName editor
-- >       lastName  <- Vertically $ field "Last:"  lastName editor
-- >       age       <- Vertically $ field "Age:"   age editor
-- >       return Person{..}
pattern Vertically :: EditorFactory a Layout b -> EditorFactory a Vertical b
pattern Vertically {vertically} <- (over editorFactoryElement getVertical -> vertically) where Vertically a = over editorFactoryElement Vertical a

-- | Applicative modifier for horizontal composition of editor factories.
--   This can be used in conjunction with ApplicativeDo as:
--
-- > editorPerson = horizontally $ do
-- >       firstName <- Horizontally $ field "First:" firstName editor
-- >       lastName  <- Horizontally $ field "Last:"  lastName editor
-- >       age       <- Horizontally $ field "Age:"   age editor
-- >       return Person{..}
pattern Horizontally :: EditorFactory a Layout b -> EditorFactory a Horizontal b
pattern Horizontally {horizontally} <- (over editorFactoryElement getHorizontal -> horizontally) where Horizontally a = over editorFactoryElement Horizontal a

infixl 4 |*|, -*-
infixl 5 |*, *|, -*, *-

withLayout :: (Layout -> m) -> EditorFactory a Layout b -> EditorFactory a m b
withLayout = over editorFactoryElement

construct :: LayoutMonoid m => EditorFactory a m b -> EditorFactory a Layout b
construct = over editorFactoryElement runLayoutMonoid

-- | Left-right editor composition
(|*|) :: EditorFactory s Layout (b -> a) -> EditorFactory s Layout b -> EditorFactory s Layout a
a |*| b = over editorFactoryElement getHorizontal $ over editorFactoryElement Horizontal a <*> over editorFactoryElement Horizontal b

-- | Left-right composition of an editorElement with a editor
(*|) :: UI Element -> EditorFactory s Layout a -> EditorFactory s Layout a
e *| a = over editorFactoryElement getHorizontal $ liftElement(Horizontal . Single <$> e) *> over editorFactoryElement Horizontal a

-- | Left-right composition of an editorElement with a editor
(|*) :: EditorFactory s Layout a -> UI Element -> EditorFactory s Layout a
a |* e = over editorFactoryElement getHorizontal $ over editorFactoryElement Horizontal a <* liftElement(Horizontal . Single <$> e)

-- | Left-right editor composition
(-*-) :: EditorFactory s Layout (b -> a) -> EditorFactory s Layout b -> EditorFactory s Layout a
a -*- b = over editorFactoryElement getVertical $ over editorFactoryElement Vertical a <*> over editorFactoryElement Vertical b

-- | Left-right composition of an editorElement with a editor
(*-) :: UI Element -> EditorFactory s Layout a -> EditorFactory s Layout a
e *- a = over editorFactoryElement getVertical $ liftElement(Vertical . Single <$> e) *> over editorFactoryElement Vertical a

-- | Left-right composition of an editorElement with a editor
(-*) :: EditorFactory s Layout a -> UI Element -> EditorFactory s Layout a
a -* e = over editorFactoryElement getVertical $ over editorFactoryElement Vertical a <* liftElement(Vertical . Single <$> e)

-- | A helper that arranges a label with the field name
--   and the editor horizontally.
fieldLayout :: LayoutMonoid m => (Layout -> m ) -> String -> (out -> inn) -> EditorFactory inn Layout a -> EditorFactory out m a
fieldLayout l name f e = withLayout l (string name *| dimapEF f id e)

field :: String -> (out -> inn) -> EditorFactory inn Layout a -> EditorFactory out Layout a
field name f e = string name *| dimapEF f id e

editorUnit :: EditorFactory b Layout b
editorUnit = EF $ \b -> do
    t <- new
    return $ Editor (tidings b never) (Single t)

-- An editor that presents a choice of values.
editorEnumBounded
  :: (Bounded a, Enum a, Ord a, Show a)
  => Behavior(a -> UI Element) -> EditorFactory (Maybe a) Layout (Maybe a)
editorEnumBounded = editorSelection (pure $ enumFrom minBound)

-- | An editor that presents a dynamic choice of values.
editorSelection
  :: Ord a
  => Behavior [a] -> Behavior(a -> UI Element) -> EditorFactory (Maybe a) Layout (Maybe a)
editorSelection options display = EF $ \b -> do
  l <- listBox options b display
  return $ Editor (tidings b (rumors $ userSelection l)) (Single $ getElement l)

-- | Ignores 'Nothing' values and only updates for 'Just' values
editorJust :: EditorFactory (Maybe b) el (Maybe b) -> EditorFactory b el b
editorJust (EF editor) = EF $ \b -> do
  e <- editor (Just <$> b)
  let ev = filterJust (edited e)
  return $ Editor (tidings b ev) (_editorElement e)

-- | An editor for union types, built from editors for its constructors.
editorSum
  :: (Ord tag, Show tag)
  => (Layout -> Layout -> Layout) -> [(tag, EditorFactory a Layout a)] -> (a -> tag) -> EditorFactory a Layout a
editorSum combineLayout options selector = EF $ \ba -> do
  options <- mapM (\(tag, EF mk) -> (tag,) <$> (mk ba >>= layoutEditor)) options
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

editorIdentity :: EditorFactory a el a -> EditorFactory (Identity a) el (Identity a)
editorIdentity = dimapEF runIdentity Identity
