{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Graphics.UI.Threepenny.Editors.Layout
  (
    Layout(Grid, Single)
  , horizontal
  , vertical
  , runLayout
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

vertical, horizontal :: Layout -> Layout -> Layout
vertical (Grid rows@(length.head.toList -> l1)) (Grid rows'@(length.head.toList -> l2)) =
    Grid $ fmap pad1 rows <> fmap pad2 rows'
  where
    pad l1 l2 | l1 >= l2  = id
              | otherwise = (<> Seq.replicate (l2-l1) Nothing)
    pad1 = pad l1 l2
    pad2 = pad l2 l1

horizontal (Grid rows@(length -> l1)) (Grid rows'@(length -> l2)) =
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
