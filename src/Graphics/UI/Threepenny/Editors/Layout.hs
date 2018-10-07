{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

-- | A custom layout engine and combinators.
module Graphics.UI.Threepenny.Editors.Layout
  (
  -- * Renderableable widgets
    Renderable(..)
  -- * Layout engine
  , Layout(Grid, Single)
  , beside
  , above
  -- * Layout monoids
  -- ** Flat
  , Vertical(..)
  , vertical
  , Horizontal(..)
  , horizontal
  -- ** Columns
  , Columns (Break, Next)
  -- * Type level layouts
  , type (|*|)(..)
  , type (-*-)(..)
  ) where

import           Control.Monad
import           Data.Biapplicative
import           Data.Bifoldable
import           Data.HasEmpty
import           Data.Foldable                   (Foldable(foldMap))
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import           Data.Monoid                     (Monoid(..))
import           Data.Ord
import           Data.Semigroup
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Tuple
import           Generics.SOP.TH
import           GHC.Exts                        (IsList (..))
import           Graphics.UI.Threepenny.Core     as UI hiding (empty)
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Widgets
import           Text.Show
import           Prelude(Num(..), Int, String, otherwise)

-- | Closely related to 'Widget', this class represents types that can be rendered to an 'Element', either directly or via 'Layout'.
class Renderable w where
  render    :: w -> UI Element
  getLayout :: w -> Layout
  render = runLayout . getLayout
  getLayout = Cell . Just . render
  {-# MINIMAL render | getLayout #-}

instance Renderable Element where
  render = return

instance Renderable a => Renderable (UI a) where
  render = (>>= render)

instance Renderable TextEntry where
  render = return . getElement

instance Renderable (ListBox a) where
  render = return . getElement

instance Renderable String where
  render = string

-- | A rathe limited, grid layout builder, probably not fit for general purpose use yet.
data Layout
  = Grid (Seq (Seq Layout)) -- ^ A non empty list of rows, where all the rows are assumed to have the same length
  | Cell (Maybe (UI Element))

pattern HasEmpty :: Layout
pattern HasEmpty = Cell Nothing

pattern Single :: UI Element -> Layout
pattern Single x = Cell (Just x)

above, beside :: Layout -> Layout -> Layout
above (Grid rows@(length.head.toList -> l1)) (Grid rows'@(length.head.toList -> l2)) =
    Grid $ fmap pad1 rows <> fmap pad2 rows'
  where
    pad l1 l2 | l1 >= l2  = id
              | otherwise = (<> Seq.replicate (l2-l1) (Cell Nothing))
    pad1 = pad l1 l2
    pad2 = pad l2 l1
above x (Cell Nothing) = x
above (Cell Nothing) x = x
above cell@(Cell Just{}) x = above (Grid [[cell]]) x
above x cell@(Cell Just{}) = above x (Grid [[cell]])

beside (Grid rows@(length -> l1)) (Grid rows'@(length -> l2)) =
  Grid $ Seq.zipWith (<>) (pad1 rows) (pad2 rows')
  where
    pad l1 l2
      | l1 >= l2  = id
      | otherwise = \x ->
          let padding = Seq.replicate (length $ head $ toList x) (Cell Nothing)
          in x <> Seq.replicate (l2 - l1) padding
    pad1 = pad l1 l2
    pad2 = pad l2 l1
beside x (Cell Nothing) = x
beside (Cell Nothing) x = x
beside cell@(Cell Just{}) x = beside (Grid [[cell]]) x
beside x cell@(Cell Just{}) = beside x (Grid [[cell]])

instance Renderable Layout where
  getLayout = id

runLayout :: Layout -> UI Element
runLayout (Grid rows) = grid (toList $ fmap (fmap runLayout . toList) rows)
runLayout (Cell el)   = fromMaybe new el

-- | A monoidal layout builder that places everything in a single column
newtype Vertical = Vertical { getVertical :: Layout}

vertical :: Renderable w => w -> Vertical
vertical = Vertical . getLayout

instance Semigroup Vertical where
  Vertical a <> Vertical b = Vertical $ above a b

instance Monoid Vertical where
  mempty = Vertical HasEmpty
  mappend = (<>)

instance Renderable Vertical where
  getLayout = getVertical

-- | A monoidal layout builder that places everything in a single row
newtype Horizontal = Horizontal { getHorizontal :: Layout}

horizontal :: Renderable w => w -> Horizontal
horizontal = Horizontal . getLayout

instance Semigroup Horizontal where
  Horizontal a <> Horizontal b = Horizontal $ beside a b

instance Monoid Horizontal where
  mempty = Horizontal HasEmpty
  mappend = (<>)

instance Renderable Horizontal where
  getLayout = getHorizontal

-- | A monoidal layout builder that lays elements in columns
data Columns
  = Next  Layout -- ^ Continue in the same column
  | Break Layout -- ^ Continue in the next column
  | Columns { next    :: (Int, Int)            -- ^ (row, column)
            , acc     :: Map (Int, Int) Layout
            }

instance Renderable Columns where
  getLayout = layoutColumns

instance Show Columns where
  show(Next _)   = "Next"
  show(Break _)  = "Break"
  show Columns{..} = unwords ["Columns", show next, show (Map.keys acc)]

layoutColumns :: Columns -> Layout
layoutColumns (Next l) = l
layoutColumns (Break l) = l
layoutColumns Columns{acc}
  | Map.null acc = HasEmpty
  | otherwise =
    getLayout $
    foldMap Vertical
      [ getLayout $
        foldMap Horizontal $ catMaybes [Map.lookup (i, j) acc | j <- [0 .. c]]
        | i <- [0 .. r]
      ]
  where
    r = maximum $ fst <$> Map.keys acc
    c = maximum $ snd <$> Map.keys acc

instance Monoid Columns where
  mempty = Columns (-1,-1) mempty
  mappend = (<>)

instance Semigroup Columns where
  Next a <> Columns (r,c) g = let xy = (r+1, max 0 c) in Columns xy (Map.insert xy a g)
  Break a <> Columns (_,c) g = let xy = (0, c + 1) in Columns xy (Map.insert xy a g)
  -- merging two columns should not ever happen, but if it does we will merge the columns and Break into a new one
  Columns (r,c) g <> Columns (r',_) g' = Columns (r+r'+1, -1) (Map.union g (Map.mapKeys (\(x,y) -> (x+r+1,y+c+1)) g'))
  c@Columns{} <> other = mappend other c
  -- Next and Break merges should not arise in practice either
  Next  a <> Next  b = Columns ( 1,0) (Map.fromList [((0,0),a), ((1,0),b)])
  Next  a <> Break b = Columns ( 0,1) (Map.fromList [((0,0),a), ((0,1),b)])
  Break a <> Break b = mappend (Next a) (Break b)
  Break a <> Next  b = mappend (Next a) (Next  b)

-- | Type level Horizontal layouts
data a |*| b = a :|*| b

instance Bifunctor (|*|) where
  bimap f g (a :|*| b) = f a :|*| g b

instance Bifoldable (|*|) where
  bifoldMap f g (a :|*| b) = f a `mappend` g b

instance Biapplicative (|*|) where
  bipure a b = a :|*| b
  fa :|*| fb <<*>> a :|*| b = fa a :|*| fb b

instance (Renderable a, Renderable b) => Renderable (a |*| b) where
  getLayout (a :|*| b) = getLayout a `beside` getLayout b

instance (HasEmpty a, HasEmpty b) => HasEmpty (a |*| b) where emptyValue = emptyValue :|*| emptyValue

-- | Type level Vertical layouts
data a -*- b = a :-*- b

instance Bifunctor (-*-) where
  bimap f g (a :-*- b) = f a :-*- g b

instance Bifoldable (-*-) where
  bifoldMap f g (a :-*- b) = f a `mappend` g b

instance Biapplicative (-*-) where
  bipure a b = a :-*- b
  fa :-*- fb <<*>> a :-*- b = fa a :-*- fb b

instance (Renderable a, Renderable b) => Renderable (a -*- b) where
  getLayout (a :-*- b) = getLayout a `above` getLayout b

instance (HasEmpty a, HasEmpty b) => HasEmpty (a -*- b) where emptyValue = emptyValue :-*- emptyValue

deriveGeneric ''(|*|)
deriveGeneric ''(-*-)
