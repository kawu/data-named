-- | Working with NE trees and forests.

module Data.Named.Tree
( 
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
, mapForest
, mapTree
, onLeaf
, onNode
, onEither
, onBoth
) where

import Prelude hiding (span)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ix (Ix, range)
import qualified Data.Tree as T
import qualified Data.Set as S

-- | Map function over the leaf value.
onLeaf :: (a -> b) -> Either c a -> Either c b
onLeaf _ (Left x)  = Left x
onLeaf f (Right x) = Right (f x)
{-# INLINE onLeaf #-}

-- | Map function over the internal node value.
onNode :: (a -> b) -> Either a c -> Either b c
onNode f (Left x)  = Left (f x)
onNode _ (Right x) = Right x
{-# INLINE onNode #-}

-- | Map the first function over internal node value
-- and the second one over leaf value.
onEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
onEither f _ (Left x)  = Left (f x)
onEither _ g (Right x) = Right (g x)
{-# INLINE onEither #-}

-- | Map one function over both node and leaf values.
onBoth :: (a -> b) -> Either a a -> Either b b
onBoth f (Left x)  = Left (f x)
onBoth f (Right x) = Right (f x)
{-# INLINE onBoth #-}

-- | Map function over each tree from the forest.
mapForest :: (a -> b) -> T.Forest a -> T.Forest b
mapForest = map . mapTree
{-# INLINE mapForest #-}

-- | Map function over the tree.
mapTree :: (a -> b) -> T.Tree a -> T.Tree b
mapTree = fmap
{-# INLINE mapTree #-}

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
