{-# LANGUAGE DoAndIfThenElse #-}

-- | Implementation of a graph with each node identified by a unique key.
-- It is a provisional module and it might be replaced by the standard
-- graph from containers package in the future.

module Data.Named.Graph
( Graph (..)
, mkGraph
, node
, edges
, roots
, disjointForest
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

import Data.Named.Tree

-- | A graph.
data Graph k v = Graph
    { nodeMap :: M.Map k v
    , edgeMap :: M.Map k [k] }

-- | Make a graph from a list of (key, value, [children keys]) tuples.
mkGraph :: Ord k => [(k, v, [k])] -> Graph k v
mkGraph xs = 
    Graph ns es
  where
    ns = M.fromList [(k, v)  | (k, v, _)  <- xs]
    es = M.fromList [(k, ks) | (k, _, ks) <- xs]

-- | Get node with the given key.
node :: (Show k, Ord k) => Graph k v -> k -> v
node g k = case M.lookup k (nodeMap g) of
    Nothing -> error $ "node: key " ++ show k ++ " not in the nodes map"
    Just v  -> v
{-# INLINE node #-}

-- | Get keys of adjacent nodes for the given node key.
edges :: (Show k, Ord k) => Graph k v -> k -> [k]
edges g k = case M.lookup k (edgeMap g) of
    Nothing -> error $ "edges: key " ++ show k ++ " not in the edges map"
    Just v  -> v
{-# INLINE edges #-}

-- | Return all graph roots (i.e. nodes with no parents).
roots :: Ord k => Graph k v -> [k]
roots g =
    let desc = S.fromList . concat . M.elems $ edgeMap g
    in  [k | (k, _) <- M.assocs (nodeMap g), not (k `S.member` desc)]

generate :: (Show k, Ord k) => Graph k v -> k -> T.Tree k
generate g k  = T.Node k (map (generate g) (edges g k))

-- | A stateful monad for forest pruning.
newtype RanM a = RanM { runRanM :: Int -> (a, Int) }

instance Monad RanM where
    return x     = RanM $ \s -> (x, s)
    RanM v >>= f = RanM $ \s -> case v s of (x, s') -> runRanM (f x) s'

run :: RanM a -> a
run act = fst (runRanM act (-1))

contains :: Int -> RanM Bool
contains k = RanM $ \m -> (k <= m, m)

include :: Int -> RanM ()
include k = RanM $ \_ -> ((), k)

chop :: T.Forest (k, Span) -> RanM (T.Forest (k, Span))
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

prune :: (k -> Int) -> T.Forest k -> T.Forest k
prune f = unSpanForest . run . chop . sortForest . spanForest f

-- | Spanning-like forest of a DAG.  Trees in the resulting forest are
-- disjoint with respect to their ranges.  It is not checked if the input
-- graph is actually a DAG.
disjointForest :: (Show k, Ord k) => (k -> Int) -> Graph k v -> T.Forest k
disjointForest f g = prune f . map (generate g) . roots $ g
