
module Graph
(
  Node,
  Graph,
  createGraph,
  buildNode,
  printGraph,
  getIdsFromAssocs,
  buildNodes,
  targetExistsInGraph,
  nodeExistsInList,
  graphNodesFromValues,
  filterNodeNeighbors,
  updateDistanceAndPredecessor,
  bfs,
  shortestDistance,
  nodesFromBfs,
  doTheMagic
) where

import Association
import qualified Data.Set as Set


data Node = Node {
  nodeValue :: Int,
  nodeEdges :: [Int],
  nodeDistanceFromRoot :: Int,
  nodePredecessor :: Int
} deriving (Show)

data Graph = Graph [Node] deriving (Show)



getIdsFromAssocs :: [Association] -> [Int]
getIdsFromAssocs assocs = Set.toList $ Set.fromList $ map (\a -> associationFirstUserId a) assocs

buildNodes :: [Int] -> [Association] -> [Node]
buildNodes values assocs =
    map (\v -> buildNode (v) (filterAssocs v assocs)) values


filterAssocs :: Int -> [Association] -> [Association]
filterAssocs value assocs =
    filter (\a -> associationFirstUserId a == value) assocs

buildNode value assocs =
  Node value [associationSecondUserId assoc | assoc <- assocs ] 0 0

createGraph :: [Node] -> Graph
createGraph nodes = Graph nodes

doTheMagic :: Int -> Int -> [Association] -> [Int]
doTheMagic fromUser toUser assocs = do
  let graph = createGraph $ buildNodes ((getIdsFromAssocs $ (assocs))) (assocs)
  let queue = graphNodesFromValues graph [fromUser]
  let out = Graph queue
  let seen = queue
  let res = shortestDistance graph out queue seen toUser
  let edges = nodesFromBfs res fromUser toUser []
  edges

-- Imprime cada nó de um grafo.
printGraph :: Graph -> IO ()
printGraph (Graph []) = putStrLn ""
printGraph (Graph(x:y)) = do
  print x
  printGraph (Graph y)
  return ()

-- Chamar antes de rodar o shortestDistance. Se o target não existir no grafo gerado pelo nó de início, já pode parar a execução.
targetExistsInGraph :: Int -> Graph -> Bool
targetExistsInGraph _ (Graph []) = False
targetExistsInGraph target (Graph (a:b)) = foldl (\acc x -> nodeValue x == target || acc) False (a:b)

-- Usada pra conferir se um node já existe na lista de nodes visitados.
nodeExistsInList :: Node -> [Node] -> Bool
nodeExistsInList _ [] = False
nodeExistsInList Node {nodeValue = value} (x:y) = foldl (\ acc x -> nodeValue x == value || acc) False (x:y)

-- Recebe uma lista de valores e retorna os data Nodes correspondentes que existem no grafo.
graphNodesFromValues :: Graph -> [Int] -> [Node]
graphNodesFromValues (Graph []) _ = []
graphNodesFromValues (Graph (x:y)) [] = x : y
graphNodesFromValues (Graph (x:y)) keys = filter (\z -> nodeValue z `elem` keys) (x:y)

-- Usada para evitar enfileirar o mesmo valor mais de uma vez.
filterNodeNeighbors :: [Node] -> [Node] -> [Node]
filterNodeNeighbors _ [] = []
filterNodeNeighbors [] _ = []
filterNodeNeighbors seenNodes nodeNeighbors = filter (\x -> not $ nodeExistsInList x seenNodes) nodeNeighbors

-- Atualiza a distância da raiz e o node anterior da busca para uma lista de nodes.
-- O valor da distância nem será usado, mas o node anterior é usado para reconstruir o path depois da execução da busca.
updateDistanceAndPredecessor :: [Node] -> Int -> Int -> [Node]
updateDistanceAndPredecessor [] _ _ = []
updateDistanceAndPredecessor (x:y) dist predValue = map (\ (Node value n _ _) -> Node value n dist predValue) (x:y)

-- Breadth-First Search recursiva, utilizada com base para o algoritmo de shortest path.
-- Opera sobre o grafo inteiro a partir de um nó inicial e retorna outro grafo com
-- a distância atualizada de cada nó do grafo até ele.
--     Input    Output   Queue     Visited   Output
bfs :: Graph -> Graph -> [Node] -> [Node] -> Graph
bfs (Graph []) _ _ _ = Graph []
bfs _ outGraph [] _ = outGraph
bfs inGraph (Graph (a:b)) (c:d) seen = bfs inGraph outGraph queue seen'
  where curValue = nodeValue c
        curNeighbors = nodeEdges c
        curNodeNeighbors = graphNodesFromValues inGraph curNeighbors
        dist = nodeDistanceFromRoot c + 1
        filteredNeighbors = filterNodeNeighbors seen curNodeNeighbors
        enqueue = updateDistanceAndPredecessor filteredNeighbors dist curValue
        outGraph = Graph $ (a:b) ++ enqueue
        queue = d ++ enqueue
        seen' = seen ++ enqueue

-- Adaptação da função acima para receber um valor alvo que se deseja buscar. Assim que esse valor for
-- adicionado à lista de nodes visitados, a execução retorna. Se supõe que o nó existe no grafo.
-- Para garantir isso, usar a função targetExistsInGraph antes.
--                  Input    Output   Queue     Visited  Target
shortestDistance :: Graph -> Graph -> [Node] -> [Node] -> Int -> Graph
shortestDistance (Graph []) _ _ _ _ = Graph []
shortestDistance _ outGraph [] _ _ = outGraph
shortestDistance inGraph (Graph (a:b)) (c:d) seen target
  | nodeExistsInList targetNode seen' == True = outGraph
  | otherwise = shortestDistance inGraph outGraph queue seen' target
  where targetNode = graphNodesFromValues inGraph [target] !! 0
        curValue = nodeValue c
        curNeighbors = nodeEdges c
        curNodeNeighbors = graphNodesFromValues inGraph curNeighbors
        dist = nodeDistanceFromRoot c + 1
        filteredNeighbors = filterNodeNeighbors seen curNodeNeighbors
        enqueue = updateDistanceAndPredecessor filteredNeighbors dist curValue
        outGraph = Graph $ (a:b) ++ enqueue
        queue = d ++ enqueue
        seen' = seen ++ enqueue

-- Usada para obter o caminho de um node a outro em cima do grafo gerado pelo bfs/shortestDistance.
--          grafo bfs - start - target - edges - edges
nodesFromBfs :: Graph -> Int -> Int -> [Int] -> [Int]
nodesFromBfs graph from to nodes
  | from == to = (nodeValue fromNode):nodes
  | otherwise = nodesFromBfs graph from (nodePredecessor toNode) nodes'
  where toNode = graphNodesFromValues graph [to] !! 0
        fromNode = graphNodesFromValues graph [from] !! 0
        nodes' = (nodeValue toNode):nodes