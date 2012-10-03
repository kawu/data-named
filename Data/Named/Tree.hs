-- | Working with NE trees and forests.

module Data.Named.Tree
( 
-- * Combine with words
  addWords

-- * Span
, Span (..)
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
, mapTrees
) where

import Prelude hiding (span)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Tree as T
import qualified Data.IntSet as S
import qualified Data.Map as M

-- | Map function over each tree from the forest.
mapTrees :: (a -> b) -> T.Forest a -> T.Forest b
mapTrees f = map (fmap f)

-- | Spanning of a tree/forest.
data Span = Span
    { beg   :: Int
    , end   :: Int }
    deriving (Show, Eq, Ord)

leafSpan :: Int -> Span
leafSpan i = Span i i

(<>) :: Span -> Span -> Span
Span p q <> Span p' q' = Span (min p p') (max q q')

spanSet :: Span -> S.IntSet
spanSet s = S.fromList [beg s .. end s]

-- | Get span of the span-annotated tree.
span :: T.Tree (a, Span) -> Span
span = snd . T.rootLabel

-- | Annotate tree nodes with spanning info given the function
-- which assignes indices to leaf nodes.
spanTree :: (k -> Int) -> T.Tree k -> T.Tree (k, Span)
spanTree f (T.Node k []) = T.Node (k, leafSpan (f k)) []
spanTree f (T.Node k ts) =
    let us = spanForest f ts
        s  = foldl1 (<>) (map span us)
    in  T.Node (k, s) us

-- | Annotate forest nodes with spanning info.
spanForest :: (k -> Int) -> T.Forest k -> T.Forest (k, Span)
spanForest f ts = map (spanTree f) ts

unSpanTree :: T.Tree (k, Span) -> T.Tree k
unSpanTree = fmap fst

unSpanForest :: T.Forest (k, Span) -> T.Forest k
unSpanForest = map unSpanTree

-- | Sort the tree with respect to spanning info.
sortTree :: T.Tree (k, Span) -> T.Tree (k, Span)
sortTree (T.Node x ts) = T.Node x (sortForest ts)

-- | Sort the forest with respect to spanning info.
sortForest :: T.Forest (k, Span) -> T.Forest (k, Span)
sortForest = sortBy (comparing span) . map sortTree

-- | Combine the disjoint forest with the list of words.
-- Discontinuities will be patched with no trace.
addWords :: Ord k => T.Forest k -> [k] -> T.Forest k
addWords [] xs = [T.Node x [] | x <- xs]
addWords ts xs
    = unSpanForest . T.subForest
    . sortTree . fillTree
    . dummyRoot
    . spanForest f $ ts
  where
    f = (M.!) $ M.fromList (zip xs [0..])
    g = (M.!) $ M.fromList (zip [0..] xs)

    dummyRoot = T.Node (undefined, bounds)
    bounds = Span 0 (length xs - 1)

    fillForest = map fillTree
    fillTree (T.Node n []) = T.Node n []
    fillTree (T.Node (k, s) us) =
        let m = spanSet s S.\\ S.unions (map (spanSet . span) us)
            mkLeaf i = T.Node (g i, leafSpan i) []
        in  T.Node (k, s) (fillForest us ++ map mkLeaf (S.toList m))
