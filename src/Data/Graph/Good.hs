{-# OPTIONS_GHC -Wall #-}

module Data.Graph.Good
  ( Graph
  , graphFromEdges
  , vertices
  , edges
  , outdegree
  , indegree
  , transposeG
  , dfs
  , dff
  , topSort
  , reverseTopSort
  , components
  , scc
  , bcc
  , reachable
  , path
  ) where

import           Control.Applicative (empty)
import           Control.Arrow ((***))
import           Control.Monad ((<=<))
import           Data.Array (Ix, Array)
import qualified Data.Array as A
import qualified Data.Graph as G
import           Data.Maybe (mapMaybe, fromMaybe)


data Graph v = Graph
  { g_graph :: G.Graph
  , g_from_vert :: G.Vertex -> v
  , g_to_vert :: v -> Maybe G.Vertex
  }


graphFromEdges :: Ord v => [(v, [v])] -> Graph v
graphFromEdges vs =
  let (g, v_func, l) = G.graphFromEdges $ fmap (\(v, es) -> (v, v, es)) vs
   in Graph g (\vert -> let (v, _, _) = v_func vert in v) l


vertices :: Graph v -> [v]
vertices g = fromVertices g $ overGraph G.vertices g


edges :: Graph v -> [(v, v)]
edges g = fmap (g_from_vert g *** g_from_vert g) $ overGraph G.edges g


overGraph :: (G.Graph -> r) -> Graph v -> r
overGraph f = f . g_graph


lookupArr :: Ix k => Array k v -> k -> Maybe v
lookupArr arr ix =
  let (lo, hi) = A.bounds arr
   in case (lo <= ix && ix <= hi) of
        True -> Just $ arr A.! ix
        False -> Nothing


outdegree :: Graph v -> v -> Maybe Int
outdegree g = lookupArr arr <=< g_to_vert g
  where
    arr = overGraph G.outdegree g


indegree :: Graph v -> v -> Maybe Int
indegree g = lookupArr arr <=< g_to_vert g
  where
    arr = overGraph G.indegree g


transposeG :: Graph v -> Graph v
transposeG g = g { g_graph = overGraph G.transposeG g }


fromVertices :: Functor f => Graph v -> f G.Vertex -> f v
fromVertices = fmap . g_from_vert


dfs :: Graph v -> [v] -> G.Forest v
dfs g vs =
  let verts = mapMaybe (g_to_vert g) vs
   in fmap (fromVertices g) $ overGraph G.dfs g verts


dff :: Graph v -> G.Forest v
dff g = fmap (fromVertices g) $ overGraph G.dff g


topSort :: Graph v -> [v]
topSort g = fromVertices g $ overGraph G.topSort g


reverseTopSort :: Graph v -> [v]
reverseTopSort = reverse . topSort


components :: Graph v -> G.Forest v
components g = fmap (fromVertices g) $ overGraph G.components g


scc :: Graph v -> G.Forest v
scc g = fmap (fromVertices g) $ overGraph G.scc g


bcc :: Graph v -> G.Forest [v]
bcc g = fmap (fmap $ fromVertices g) $ overGraph G.bcc g


reachable :: Graph v -> v -> [v]
reachable g v = case g_to_vert g v of
  Nothing -> empty
  Just vert -> fromVertices g $ overGraph G.reachable g vert


path :: Graph v -> v -> v -> Bool
path g v1 v2 = fromMaybe False $ do
  vert1 <- g_to_vert g v1
  vert2 <- g_to_vert g v2
  pure $ overGraph G.path g vert1 vert2

