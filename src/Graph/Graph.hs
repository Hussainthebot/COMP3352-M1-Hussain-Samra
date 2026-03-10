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

-- a simple vertex -> edges representation of the Graph
newtype Graph v = Graph { vertices :: HM.HashMap v (HS.HashSet v) } deriving (Eq)

instance (Eq v, Hashable v, Show v) => Show (Graph v) where
  show g = "Graph.fromList " ++ show (toList g)

-- construct the empty graph
empty :: Graph v
empty = Graph { vertices = HM.empty }

-- add a vertex with a list of edges to the graph
addVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
addVertex vertex edgelist g@(Graph vertices) =
  -- see if we have the vertex, and if so modify it
  case getVertexEdges vertex g of
    -- we found it, so combine the sets of edges
    Just edges ->
      -- little shortcut if the edge list is empty
      if null edgelist then g
      else Graph $ HM.insert vertex (edges `HS.union` HS.fromList edgelist) vertices

    -- otherwise, insert it
    Nothing -> Graph $ HM.insert vertex (HS.fromList edgelist) vertices

-- deletes a vertex from the graph
deleteVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
deleteVertex vertex (Graph vertices) =
  Graph $ HM.delete vertex vertices

-- deletes an edge from the Graph
deleteEdge ::  (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteEdge fromVertex toVertex (Graph vertices) =
  Graph $ HM.adjust (HS.delete toVertex) fromVertex vertices

-- deletes an undirected edge from the graph
deleteUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteUndirectedEdge fromVertex toVertex graph =
  deleteEdge toVertex fromVertex $ deleteEdge fromVertex toVertex graph

-- add a vertex with a list of edges to the graph
replaceVertex :: (Eq v, Hashable v) => v -> [v] -> Graph v -> Graph v
replaceVertex vertex edgelist graph =
  -- see if we have the vertex, and if so modify it
  addVertex vertex edgelist $ deleteVertex vertex graph

-- add an edge to our graph
addEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addEdge fromVertex toVertex (Graph vertices) =
  case HM.lookup fromVertex vertices of
    Nothing -> Graph $ HM.insert fromVertex (HS.singleton toVertex) vertices
    Just edges ->
      Graph $ HM.insert fromVertex (HS.insert toVertex edges) vertices

-- adds an edge in both directions between two vertices
addUndirectedEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
addUndirectedEdge fromVertex toVertex g =
  addEdge toVertex fromVertex (addEdge fromVertex toVertex g)

-- returns true or false if this vertex exists
hasVertex :: (Eq v, Hashable v) => v -> Graph v -> Bool
hasVertex vertex (Graph vertices) = HM.member vertex vertices

-- returns true or fals if this edge exists
hasEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
hasEdge fromVertex toVertex graph =
  case getVertexEdges fromVertex graph of
    Just edges -> HS.member toVertex edges
    Nothing -> False

-- returns a list of vertices in the graph (O(n), but the constant
-- is pretty high since we have to create a list)
getVertices :: (Eq v, Hashable v) => Graph v -> [v]
getVertices (Graph vertices) = HM.keys vertices


-- returns the vertices that match a predicate in the form of
-- a list of (vertex, HashSet vertex) pairs, note that the 2nd part
-- of this type is a HashSet of the vertices this vertex is connected to
filterVertices :: (Eq v, Hashable v) => (v -> Bool) -> Graph v -> [(v, HS.HashSet v)]
filterVertices f (Graph vertices) =
  L.filter (\(v, _) -> f v) (HM.toList vertices)

-- searches for the vertex and returns a HashSet of edges
getVertexEdges :: (Eq v, Hashable v) => v -> Graph v -> Maybe (HS.HashSet v)
getVertexEdges v (Graph vertices) = HM.lookup v vertices

-- construct a graph from an adjacency list, i.e., pairs of vertex and edge lists
fromList :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromList [] = empty
fromList lst = L.foldl' (\acc (v, es) -> addVertex v es acc) empty lst

-- constructs an undirected graph, i.e., edges go both ways
fromListUndirected :: (Eq v, Hashable v) => [(v, [v])] -> Graph v
fromListUndirected [] = empty
fromListUndirected lst =
  L.foldl' (\g (v, es) ->
    -- slightly tricky, undirected edge takes two vertices and a graph
    -- so we give it the first vertex from the outer fold, then flip the
    -- input arguments--this is because foldl' gives us (\g v -> ...) as
    -- the arguments, so we use flip to reverse these on addUndirected edge
    L.foldl' (flip (addUndirectedEdge v)) g es) empty lst

-- converts the graph into an association list representation, i.e., a
-- list of (v, [v]) pairs, where v is the vertex and [v] is the list of
-- vertices we are connected
toList :: (Eq v, Hashable v) => Graph v -> [(v, [v])]
toList (Graph vertices) =
  let lst = HM.toList vertices in
    -- this is the only 'clever' thing here
    map (Data.Bifunctor.second HS.toList) lst
