module Graph.Graph where
{-
  This module implements a simple edge-list Graph representation. It's simple
  in that there isn't really an edge type, so edges are unlabeled. 

  *Note* This module depends on:

  - unordered-containers
  - hashable

  meaning that this must be in your build, such as your package.yaml file
  if you're using Stack. unordered-containers gives us hash tables, while
  hashable gives us a Hashable typeclass.

  Any vertex you put in here _must_ be in the Hashable type class so that
  it can be hashed and put into the underlying containers.

  Limitations: You cannot have multiple edges between the same two vertices
  in the same direction. An edge-list representation should allow this, but
  for simplicity, we don't.
-}

-- the graph will be represented as a set of
-- vertices to an adjacency map
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Data.Hashable
import qualified Data.List as L
import Data.Bifunctor (second)

-- | A directed graph represented as a HashMap from each vertex to the set of
-- vertices it has outgoing edges to. Using a HashSet for adjacency prevents
-- duplicate edges between the same pair of vertices in the same direction.
newtype Graph v = Graph { vertices :: HM.HashMap v (HS.HashSet v) } deriving (Eq)

-- | Show instance walks the graph via toList, producing a string that can be
-- copy-pasted back into source as a call to fromList to reconstruct the graph.
instance (Eq v, Hashable v, Show v) => Show (Graph v) where
  show g = "Graph.fromList " ++ show (toList g)

-- | The empty graph: no vertices, no edges.
empty :: Graph v
empty = Graph { vertices = HM.empty }

-- | Add a vertex with an initial list of outgoing edge targets.
-- If the vertex already exists, the new edges are *unioned* into its existing
-- edge set (no duplicates, existing edges are preserved).
-- If the edge list is empty and the vertex already exists, the graph is
-- returned unchanged as a short-circuit optimisation.
addVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
addVertex vertex edgelist g@(Graph vertices) =
  -- see if we have the vertex, and if so modify it
  case getVertexEdges vertex g of
    -- we found it, so combine the sets of edges
    Just edges ->
      -- little shortcut if the edge list is empty
      if null edgelist then g
      else Graph $ HM.insert vertex (edges `HS.union` HS.fromList edgelist) vertices

    -- otherwise, insert it fresh with the provided edge list
    Nothing -> Graph $ HM.insert vertex (HS.fromList edgelist) vertices

-- | Remove a vertex (and all its *outgoing* edges) from the graph.
-- Note: incoming edges from other vertices that point *to* this vertex are
-- NOT removed. Use deleteEdge on those vertices separately if needed.
deleteVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
deleteVertex vertex (Graph vertices) =
  Graph $ HM.delete vertex vertices

-- | Remove a single directed edge from fromVertex to toVertex.
-- If either vertex does not exist the graph is returned unchanged.
deleteEdge ::  (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteEdge fromVertex toVertex (Graph vertices) =
  -- HM.adjust applies HS.delete only to the adjacency set of fromVertex;
  -- if fromVertex is absent, adjust is a no-op.
  Graph $ HM.adjust (HS.delete toVertex) fromVertex vertices

-- | Remove an undirected edge by deleting the directed edge in both
-- directions: fromVertex -> toVertex *and* toVertex -> fromVertex.
deleteUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteUndirectedEdge fromVertex toVertex graph =
  deleteEdge toVertex fromVertex $ deleteEdge fromVertex toVertex graph

-- | Replace a vertex's entire edge set with the supplied list.
-- This is implemented by deleting the vertex (dropping all its current edges)
-- and then re-inserting it with the new edge list via addVertex.
replaceVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
replaceVertex vertex edgelist graph =
  -- see if we have the vertex, and if so modify it
  addVertex vertex edgelist $ deleteVertex vertex graph

-- | Add a single directed edge from fromVertex to toVertex.
-- If fromVertex does not yet exist it is created with toVertex as its only
-- neighbour. If it already exists, toVertex is inserted into its edge set.
addEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addEdge fromVertex toVertex (Graph vertices) =
  case HM.lookup fromVertex vertices of
    Nothing -> Graph $ HM.insert fromVertex (HS.singleton toVertex) vertices
    Just edges ->
      Graph $ HM.insert fromVertex (HS.insert toVertex edges) vertices

-- | Add an undirected edge by calling addEdge in both directions so that
-- fromVertex -> toVertex and toVertex -> fromVertex both exist in the graph.
addUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addUndirectedEdge fromVertex toVertex g =
  addEdge toVertex fromVertex (addEdge fromVertex toVertex g)

-- | Return True if the vertex exists in the graph, False otherwise.
hasVertex :: (Eq v, Hashable v) => v -> Graph v -> Bool
hasVertex vertex (Graph vertices) = HM.member vertex vertices

-- | Return True if a directed edge from fromVertex to toVertex exists.
-- Returns False if either vertex is absent or the edge is absent.
hasEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
hasEdge fromVertex toVertex graph =
  case getVertexEdges fromVertex graph of
    Just edges -> HS.member toVertex edges
    Nothing -> False

-- | Return all vertices in the graph as a list.
-- O(n) in the number of vertices; note that converting a HashMap's keys to a
-- list carries a non-trivial constant factor due to allocation.
getVertices :: (Eq v, Hashable v) => Graph v -> [v]
getVertices (Graph vertices) = HM.keys vertices

-- | Return all (vertex, adjacencySet) pairs whose vertex satisfies predicate f.
-- The adjacency set is kept as a HashSet rather than converted to a list so
-- the caller can perform further set operations without an intermediate copy.
filterVertices :: (Eq v, Hashable v) => (v -> Bool) -> Graph v -> [(v, HS.HashSet v)]
filterVertices f (Graph vertices) =
  L.filter (\(v, _) -> f v) (HM.toList vertices)

-- | Look up the set of outgoing neighbours for a vertex.
-- Returns Nothing if the vertex is not in the graph, Just the HashSet otherwise.
getVertexEdges :: (Eq v, Hashable v) => v -> Graph v -> Maybe (HS.HashSet v)
getVertexEdges v (Graph vertices) = HM.lookup v vertices

-- | Build a directed Graph from an adjacency list of (vertex, [neighbour]) pairs.
-- Duplicate edge targets within a single pair are collapsed because the
-- underlying representation uses a HashSet.
fromList :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromList [] = empty
fromList lst = L.foldl' (\acc (v, es) -> addVertex v es acc) empty lst

-- | Build an *undirected* Graph from an adjacency list.
-- For every edge (u, v) declared in the list, edges u->v and v->u are both
-- inserted. The outer foldl' iterates over vertices; the inner foldl' iterates
-- over each vertex's declared neighbours, using `flip (addUndirectedEdge v)`
-- to satisfy foldl's (\accumulator element -> ...) argument order.
fromListUndirected :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromListUndirected [] = empty
fromListUndirected lst =
  L.foldl' (\g (v, es) ->
    -- slightly tricky, undirected edge takes two vertices and a graph
    -- so we give it the first vertex from the outer fold, then flip the
    -- input arguments--this is because foldl' gives us (\g v -> ...) as
    -- the arguments, so we use flip to reverse these on addUndirected edge
    L.foldl' (flip (addUndirectedEdge v)) g es) empty lst

-- | Convert the Graph back to an adjacency list of (vertex, [neighbour]) pairs.
-- Each vertex's HashSet of neighbours is converted to a plain list via
-- Data.Bifunctor.second, which applies the conversion to the second element of
-- every pair without touching the vertex itself.
toList :: (Eq v, Hashable v) => Graph v -> [(v, [v])]
toList (Graph vertices) =
  let lst = HM.toList vertices in
    -- this is the only 'clever' thing here
    map (Data.Bifunctor.second HS.toList) lst