-- | Working with NE trees and forests.

module Data.Named.Tree
( 
-- -- * Combine with words
--   addWords

-- * Span
  Span (..)
, leafSpan
, (<>)
, spanSet

-- * Trees with span
, span
, spanTree
, spanForest
, unSpanTree
, unSpanForest
, sortTree
, sortForest

-- * Utilities
, mapLeaves
, mapNodes
, mapTrees
) where

import Prelude hiding (span)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ix (Ix, range)
import qualified Data.Tree as T
import qualified Data.Set as S

-- | Map function over tree leaves.
mapLeaves :: (a -> b) -> T.Tree (Either c a) -> T.Tree (Either c b)
mapLeaves f (T.Node (Left x) ts) = T.Node (Left x) (map (mapLeaves f) ts)
mapLeaves f (T.Node (Right x) _) = T.Node (Right $ f x) []

-- | Map function over tree internal nodes.
mapNodes :: (a -> b) -> T.Tree (Either a c) -> T.Tree (Either b c)
mapNodes f (T.Node (Left x) ts) = T.Node (Left $ f x) (map (mapNodes f) ts)
mapNodes _ (T.Node (Right x) _) = T.Node (Right x) []

-- | Map function over each tree from the forest.
mapTrees :: (a -> b) -> T.Forest a -> T.Forest b
mapTrees f = map (fmap f)

-- | Spanning of a tree.
data Span w = Span
    { beg   :: w
    , end   :: w }
    deriving (Show, Eq, Ord)

-- | Make span for a leaf node.
leafSpan :: w -> Span w
leafSpan i = Span i i

-- | Minimum span overlapping both input spans.
(<>) :: Ord w => Span w -> Span w -> Span w
Span p q <> Span p' q' = Span (min p p') (max q q')
{-# INLINE (<>) #-}

-- | Set of positions covered by the span.
spanSet :: Ix w => Span w -> S.Set w
spanSet s = S.fromList $ range (beg s, end s)

-- | Get span of the span-annotated tree.
span :: T.Tree (a, Span w) -> Span w
span = snd . T.rootLabel

-- | Annotate tree nodes with spanning info given the function
-- which assignes indices to leaf nodes.
spanTree :: Ord w => T.Tree (Either n w) -> T.Tree (Either n w, Span w)
spanTree (T.Node (Right k) []) = T.Node (Right k, leafSpan k) []
spanTree (T.Node k ts) =
    let us = spanForest ts
        s  = foldl1 (<>) (map span us)
    in  T.Node (k, s) us

-- | Annotate forest nodes with spanning info.
spanForest :: Ord w => T.Forest (Either n w) -> T.Forest (Either n w, Span w)
spanForest = map spanTree

-- | Remove span annotations from the tree.
unSpanTree :: T.Tree (k, Span w) -> T.Tree k
unSpanTree = fmap fst

-- | Remove span annotations from the forest.
unSpanForest :: T.Forest (k, Span w) -> T.Forest k
unSpanForest = map unSpanTree

-- | Sort the tree with respect to spanning info.
sortTree :: Ord w => T.Tree (k, Span w) -> T.Tree (k, Span w)
sortTree (T.Node x ts) = T.Node x (sortForest ts)

-- | Sort the forest with respect to spanning info.
sortForest :: Ord w => T.Forest (k, Span w) -> T.Forest (k, Span w)
sortForest = sortBy (comparing span) . map sortTree
