{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Graphics.UI.Threepenny.Editors.Layout
  (
    Layout(Grid, Single)
  , beside
  , above
  , runLayout
  , Vertical(..)
  , Horizontal(..)
  ) where

import           Data.Foldable                     (length)
import           Data.Monoid
import           Data.Sequence                     (Seq, ViewL (..), viewl)
import qualified Data.Sequence                     as Seq
import           GHC.Exts                          (IsList (..))
import           Graphics.UI.Threepenny.Core       as UI
import           Graphics.UI.Threepenny.Elements

newtype Layout
  = Grid (Seq (Seq (Maybe Element))) -- ^ A non empty list of rows, where all the rows are assumed to have the same length

pattern Single :: Element -> Layout
pattern Single x <- Grid (Singleton (Singleton (Just x))) where Single x = Grid [[Just x]]

pattern Singleton :: a -> Seq a
pattern Singleton x <- (viewl -> x :< (viewl -> EmptyL)) where Singleton x = [x]

above, beside :: Layout -> Layout -> Layout
above (Grid rows@(length.head.toList -> l1)) (Grid rows'@(length.head.toList -> l2)) =
    Grid $ fmap pad1 rows <> fmap pad2 rows'
  where
    pad l1 l2 | l1 >= l2  = id
              | otherwise = (<> Seq.replicate (l2-l1) Nothing)
    pad1 = pad l1 l2
    pad2 = pad l2 l1

beside (Grid rows@(length -> l1)) (Grid rows'@(length -> l2)) =
  Grid $ Seq.zipWith (<>) (pad1 rows) (pad2 rows')
  where
    pad l1 l2
      | l1 >= l2  = id
      | otherwise = \x ->
          let padding = Seq.replicate (length $ head $ toList x) Nothing
          in x <> Seq.replicate (l2 - l1) padding
    pad1 = pad l1 l2
    pad2 = pad l2 l1

runLayout :: Layout -> UI Element
runLayout (Grid rows) = grid (toList $ fmap (fmap (maybe new return). toList) rows)

newtype Vertical = Vertical { getVertical :: Layout}

instance Monoid Vertical where
  mempty = Vertical $ Grid [[Nothing]]
  mappend (Vertical a) (Vertical b)= Vertical $ above a b

newtype Horizontal = Horizontal { getHorizontal :: Layout}

instance Monoid Horizontal where
  mempty = Horizontal $ Grid [[Nothing]]
  mappend (Horizontal a) (Horizontal b)= Horizontal $ beside a b
