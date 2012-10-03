{-# LANGUAGE DoAndIfThenElse #-}

-- | Implementation of a graph with each node identified by a unique key.

module Data.Named.Graph
( Graph (..)
, mkGraph
, edges
, roots
, toForest
) where

import Prelude hiding (span)
import Data.Either (lefts, rights)
import Data.Ix (Ix, range, inRange)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

import Data.Named.Tree

-- | A graph over a sentence.
data Graph n w = Graph
    { bounds  :: (w, w)
    , edgeMap :: M.Map n [Either n w] }

-- | Make a graph given the bounds and list of edges.
mkGraph :: (Ord n, Ix w) => (w, w) -> [(n, [Either n w])] -> Graph n w
mkGraph bs =
    Graph bs . M.fromList . map check
  where
    check (k, ks)
        | null ks =
            error "mkGraph: Left, internal node without output edges"
        | any (not . inRange bs) (rights ks) =
            error "mkGraph: Right, leaf node outside of bounds"
        | otherwise = (k, ks)

-- | Get keys of adjacent nodes for the given node key.
edges :: Ord n => Graph n w -> n -> [Either n w]
edges g k = case M.lookup k (edgeMap g) of
    Nothing -> error "edges: key not in the map"
    Just v  -> v
{-# INLINE edges #-}

-- | Return all graph roots (i.e. nodes with no parents).
roots :: Ord n => Graph n w -> [n]
roots g =
    let desc = S.fromList . lefts . concat . M.elems $ edgeMap g
    in  [k | k <- M.keys (edgeMap g), not (k `S.member` desc)]

generate :: Ord n => Graph n w -> Either n w -> T.Tree (Either n w)
generate g (Left k) = T.Node (Left k) (map (generate g) (edges g k))
generate _ w        = T.Node w []

prune :: Ord w => T.Forest (Either n w) -> T.Forest (Either n w)
prune = unSpanForest . run . chop . sortForest . spanForest

-- | Combine the disjoint forest with the list of words.
-- Discontinuities will be patched with no trace.
addWords :: Ix w => (w, w) -> T.Forest (Either n w) -> T.Forest (Either n w)
addWords (p, q) [] = [T.Node (Right x) [] | x <- range (p, q)]
addWords (p, q) ts
    = unSpanForest . T.subForest
    . sortTree . fillTree
    . dummyRoot
    . spanForest $ ts
  where
    dummyRoot = T.Node (undefined, Span p q)
    mkLeaf k  = T.Node (Right k, leafSpan k) []

    fillForest = map fillTree
    fillTree (T.Node n []) = T.Node n []
    fillTree (T.Node (k, s) us) =
        let m = spanSet s S.\\ S.unions (map (spanSet . span) us)
        in  T.Node (k, s) (fillForest us ++ map mkLeaf (S.toList m))

toForest :: (Ord n, Ix w) => Graph n w -> T.Forest (Either n w)
toForest g = addWords (bounds g) . prune . map (generate g . Left) . roots $ g

-- | A stateful monad for forest pruning.
newtype RanM w a = RanM { runRanM :: Maybe w -> (a, Maybe w) }

instance Monad (RanM w) where
    return x     = RanM $ \s -> (x, s)
    RanM v >>= f = RanM $ \s -> case v s of (x, s') -> runRanM (f x) s'

run :: RanM w a -> a
run act = fst (runRanM act Nothing)

contains :: Ord w => w -> RanM w Bool
contains k = RanM $ \m -> case m of
    Just x  -> (k <= x, m)
    Nothing -> (False,  m)

include :: w -> RanM w ()
include k = RanM $ \_ -> ((), Just k)

chop :: Ord w => T.Forest (k, Span w) -> RanM w (T.Forest (k, Span w))
chop [] = return []
chop (T.Node (k, s) ts : us) = do
    visited <- contains (end s)
    if visited then
        chop us
    else do
        as <- chop ts
        include (end s)
        bs <- chop us
        return (T.Node (k, s) as : bs)
