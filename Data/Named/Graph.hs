-- | Implementation of a graph with each node identified by a unique key.
-- It is a provisional module and it might be replaced by the standard
-- graph from containers package in the future.

module Data.Named.Graph
( Graph (..)
, mkGraph
, node
, edges
, roots
, toTree
, toKeyTree
, toForestWith
, toForest
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T
import Data.List (mapAccumL, foldl', sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.Ord (comparing)

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

-- prune :: Show k => (k -> k -> Ordering) -> T.Forest k -> T.Forest k
-- prune cmp ts = runRan cmp (chop ts)

data Ran k = Ran
    { ranKey    :: k
    , ranMax    :: k }

chop :: (k -> k -> Ordering) -> T.Forest (Ran k) -> RanM (T.Forest (Ran k))
chop cmp = doIt where
    doIt [] = return []
    doIt (Node span ts : us) = do
        outOfRange <- contains cmp (ranMax span)
        if outOfRange then
            doIt us
        else do
            include (ranMax span)
            as <- doIt ts
            bs <- doIt us
            return (Node span as : bs)

newtype RanM k a = RanM { runRanM :: k -> (a, k) }

instance Monad (RanM k) where
    return x     = RanM $ \s -> (x, s)
    RanM v >>= f = RanM $ \s -> case v s of (x, s') -> runRanM (f x) s'

-- run          :: Bounds -> RanM s a -> a
-- run _ act     = fst (runRanM act Set.empty)

contains :: (k -> k -> Ordering) -> k -> RanM k Bool
contains cmp k = RanM $ \m -> (cmp k m <= EQ, m)

include :: k -> RanM k ()
include k = RanM $ \_ -> ((), k)

-- -- | Spanning-like forest of the graph.  Trees in the resulting
-- -- forest are disjoint with respect to their ranges.
-- disjoint
--     :: (Show k, Ord k) => (k -> k -> Ordering)
--     -> Graph k v -> [k] -> T.Forest k
-- disjoint cmp g xs =
-- 
-- toForestBy
--     :: (Show k, Ord k) => (k -> k -> Ordering)
--     -> Graph k v -> T.Forest k
-- toForestBy cmp g =
--     disjoint g . sortBy cmpRoots $ roots g
--   where
--     proxy = minimumBy cmp . reachable g
--     cmpRoots r r' = cmp (proxy r) (proxy r')
