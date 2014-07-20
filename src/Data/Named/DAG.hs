{-# LANGUAGE RecordWildCards #-}

-- | Implementation of a DAG with each node identified by a unique key.

module Data.Named.DAG
( 
-- * DAG
  DAG (..)
, mkDAG
, unDAG

-- * Access by key
, vertex
, node
, edges
, maybeNode
, maybeEdges

-- * Access by vertex (index)
, nodeV
, keyV
, edgesV

-- * Conversion to forest
, toForest
, toForestBy

-- * Utilities
, roots
, leaves
) where

import Control.Applicative ((<$>))
import Data.List (sortBy, minimumBy)
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Graph as G

-- | A directed acyclic graph.
data DAG k v = DAG {
    -- | The underlying graph.
    graph       :: G.Graph,
    -- | Map vertex identifier to a node description.
    nodeDesc    :: G.Vertex -> (v, k, [k]),
    -- | Map key to a vertex identifier.  Return Nothing if the key is not
    -- a member of the graph.
    maybeVertex :: k -> Maybe G.Vertex }

-- | The node for the given key.  Return Nothing if the key is not
-- a member of the graph.
maybeNode :: DAG k v -> k -> Maybe v
maybeNode DAG{..} k = _1 . nodeDesc <$> maybeVertex k
{-# INLINE maybeNode #-}

-- | The edge list for the given key.  Return Nothing if the key is not
-- a member of the graph.
maybeEdges :: DAG k v -> k -> Maybe [k]
maybeEdges DAG{..} k = _3 . nodeDesc <$> maybeVertex k
{-# INLINE maybeEdges #-}

-- | Map key to a vertex identifier.  Report error if the key is not a member
-- of the graph.
vertex :: Show k => DAG k v -> k -> G.Vertex
vertex dag k = case maybeVertex dag k of
    Nothing -> error $ "vertex: key " ++ show k ++ " not in the graph"
    Just x  -> x

-- | The node for the given key.  Report error if the key is not a member
-- of the graph.
node :: Show k => DAG k v -> k -> v
node dag k = case maybeNode dag k of
    Nothing -> error $ "node: key " ++ show k ++ " not in the graph"
    Just x  -> x

-- | The edge list for the given key.  Report error if the key is not a member
-- of the graph.
edges :: Show k => DAG k v -> k -> [k]
edges dag k = case maybeEdges dag k of
    Nothing -> error $ "edges: key " ++ show k ++ " not in the graph"
    Just x  -> x

nodeV :: DAG k v -> G.Vertex -> v
nodeV DAG{..} = _1 . nodeDesc
{-# INLINE nodeV #-}

keyV :: DAG k v -> G.Vertex -> k
keyV DAG{..} = _2 . nodeDesc
{-# INLINE keyV #-}

edgesV :: DAG k v -> G.Vertex -> [k]
edgesV DAG{..} = _3 . nodeDesc
{-# INLINE edgesV #-}

leaves :: DAG k v -> [k]
leaves dag = [k | (_, k, []) <- unDAG dag]

roots :: Ord k => DAG k v -> [k]
roots dag =
    let desc = S.fromList . concat . map _3 $ unDAG dag
    in  [k | (_, k, _) <- unDAG dag, not (k `S.member` desc)]

-- | Smart constructur which verifies that the graph is actually a DAG.
-- Return Nothing if the input list constitutes a graph with cycles. 
mkDAG :: (Show k, Ord k) => [(v, k, [k])] -> Maybe (DAG k v)
mkDAG xs 
    | any ((>1) . length . T.flatten) (G.scc _graph) = Nothing
    | otherwise = Just $ DAG
        { graph         = _graph
        , nodeDesc      = _nodeDesc
        , maybeVertex   = _maybeVertex }
  where
    (_graph, _nodeDesc, _maybeVertex) = G.graphFromEdges xs

unDAG :: DAG k v -> [(v, k, [k])]
unDAG DAG{..} = map nodeDesc (G.vertices graph)

-- | Spanning forest of the DAG.  Non-overloaded version of the 'toForest'
-- function.  The comparison function is used to sort the list of leaves
-- and the spanning tree is computed with respect to the resulting order.
toForestBy :: (Show k, Ord k) => (k -> k -> Ordering) -> DAG k v -> T.Forest k
toForestBy cmp dag@DAG{..} =
    let proxy = minimumBy cmp . map (keyV dag)
              . G.reachable graph . vertex dag
        cmpRoots r r' = cmp (proxy r) (proxy r')
        xs = map (vertex dag) . sortBy cmpRoots $ roots dag
    in  map (fmap (_2 . nodeDesc)) (G.dfs graph xs)

-- | Spanning forest of the DAG using the standard 'compare' function to
-- compare keys kept in DAG leaves.  Overloaded version of the 'toForestBy'
-- function.
toForest :: (Show k, Ord k) => DAG k v -> T.Forest k
toForest = toForestBy compare

_1 :: (a, b, c) -> a
_1 (x, _, _) = x
{-# INLINE _1 #-}

_2 :: (a, b, c) -> b
_2 (_, x, _) = x
{-# INLINE _2 #-}

_3 :: (a, b, c) -> c
_3 (_, _, x) = x
{-# INLINE _3 #-}
