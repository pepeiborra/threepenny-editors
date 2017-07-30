{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Graphics.UI.Threepenny.Editors.Layout
  (
  -- * Layout engine
    Layout(Grid, Single)
  , beside
  , above
  , runLayout
  -- * Layout monoids
  , LayoutMonoid(..)
  -- ** Flat
  , Vertical(..)
  , Horizontal(..)
  -- ** Columns
  , Columns (Break, Next)
  ) where

import           Data.Foldable                     (length)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as Seq
import           GHC.Exts                          (IsList (..))
import           Graphics.UI.Threepenny.Core       as UI
import           Graphics.UI.Threepenny.Elements

data Layout
  = Grid (Seq (Seq Layout)) -- ^ A non empty list of rows, where all the rows are assumed to have the same length
  | Cell (Maybe Element)

pattern Empty :: Layout
pattern Empty = Cell Nothing

pattern Single :: Element -> Layout
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

runLayout :: Layout -> UI Element
runLayout (Grid rows) = grid (toList $ fmap (fmap runLayout . toList) rows)
runLayout (Cell el)   = maybe new return el

class Monoid m => LayoutMonoid m where
  runLayoutMonoid :: m -> Layout

-- | A layout monoid that places everything in a single column
newtype Vertical = Vertical { getVertical :: Layout}

instance Monoid Vertical where
  mempty = Vertical Empty
  mappend (Vertical a) (Vertical b)= Vertical $ above a b

instance LayoutMonoid Vertical where
  runLayoutMonoid = getVertical

-- | A layout monoid that places everything in a single row
newtype Horizontal = Horizontal { getHorizontal :: Layout}

instance Monoid Horizontal where
  mempty = Horizontal Empty
  mappend (Horizontal a) (Horizontal b)= Horizontal $ beside a b

instance LayoutMonoid Horizontal where
  runLayoutMonoid = getHorizontal

-- | A layout monoid that lays elements in columns
data Columns
  = Next Layout  -- ^ Continue in the same column
  | Break Layout -- ^ Continue in the next column
  | Columns { _next :: (Int, Int)            -- ^ (row, column)
            , _acc  :: Map (Int, Int) Layout
            }

instance LayoutMonoid Columns where
  runLayoutMonoid = layoutColumns

instance Show Columns where
  show(Next _) = "Next"
  show(Break _) = "Break"
  show(Columns xy m) = unwords ["Columns", show xy, show (Map.keys m)]

layoutColumns :: Columns -> Layout
layoutColumns (Next  l) = l
layoutColumns (Break l) = l
layoutColumns (Columns _ accum)
  | Map.null accum = Empty
  | otherwise =
    runLayoutMonoid $
    foldMap Vertical
      [ runLayoutMonoid $
        foldMap Horizontal $ catMaybes [Map.lookup (i, j) accum | j <- [0 .. c]]
        | i <- [0 .. r]
      ]
  where
    r = maximum $ fst <$> Map.keys accum
    c = maximum $ snd <$> Map.keys accum

instance Monoid Columns where
  mempty = Columns (-1,-1) mempty
  mappend (Next a) (Columns (r,c) g) = let xy = (r+1, max 0 c) in Columns xy (Map.insert xy a g)
  mappend (Break a) (Columns (_,c) g) = let xy = (0, c + 1) in Columns xy (Map.insert xy a g)
  -- merging two columns should not ever happen, but if it does we will merge the columns and Break into a new one
  mappend (Columns (r,c) g) (Columns (r',_) g') = Columns (r+r'+1, -1) (Map.union g (Map.mapKeys (\(x,y) -> (x+r+1,y+c+1)) g'))
  mappend c@Columns{} other = mappend other c
  -- Next and Break merges should not arise in practice either
  mappend (Next  a) (Next  b) = Columns ( 1,0) (Map.fromList [((0,0),a), ((1,0),b)])
  mappend (Next  a) (Break b) = Columns ( 0,1) (Map.fromList [((0,0),a), ((0,1),b)])
  mappend (Break a) (Break b) = mappend (Next a) (Break b)
  mappend (Break a) (Next  b) = mappend (Next a) (Next  b)
