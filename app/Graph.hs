module Graph
(
  Node,
  Graph,
) where

data Node = Node {
  nodeValue :: Int,
  nodeEdges :: [Int],
  nodeDistanceFromRoot :: Int,
  nodePredecessor :: Int
} deriving (Show)

data Graph = Graph [Node] deriving (Show)