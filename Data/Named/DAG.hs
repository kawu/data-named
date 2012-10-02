{-# LANGUAGE RecordWildCards #-}

-- | Implementation of a DAG with each node identified by a unique key.

module Data.Named.DAG
( DAG (..)
, mkDAG
, node
, edges
, toForest
) where

import Control.Applicative ((<$>))
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

-- | Spanning forest of the DAG with keys in nodes.
toForest :: Ord k => DAG k v -> T.Forest k
toForest dag = map (fmap (_2 . nodeDesc dag)) . G.dff . graph $ dag

_1 :: (a, b, c) -> a
_1 (x, _, _) = x
{-# INLINE _1 #-}

_2 :: (a, b, c) -> b
_2 (_, x, _) = x
{-# INLINE _2 #-}

_3 :: (a, b, c) -> c
_3 (_, _, x) = x
{-# INLINE _3 #-}
