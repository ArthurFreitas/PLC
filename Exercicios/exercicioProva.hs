quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (x:xs) = quicksort [y| y <- xs, y <= x] ++ [x] ++ quicksort [y| y <- xs, y > x]

f :: (Ord t) => [t] -> [t] -> [t]
f l1 l2 = removeDuplicates newList
     where newList = quicksort(l1++l2)

removeDuplicates :: (Eq t) => [t] -> [t]
removeDuplicates [] = []
removeDuplicates (x:xs) = [x] ++ removeDuplicates[y| y <- xs, x /= y]

data Graph t = Graph [t] [(t,t)] -- [t] é a lista de nós e [(t,t)] é a lista de arestas

data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq,Show)

mergeTrees :: Eq t => [Tree t] -> Graph t   -- combina duas arvores em um grafo 
mergeTrees ts = foldr insertTree (Graph [] []) ts

insertTree :: Eq t => Tree t -> Graph t -> Graph t
insertTree NilT g = grafo
insertTree tree (Graph nodes edges) = Graph(insertNodes tree notes) (insertEdges tree edges)

insertNodes :: (Eq t) => (Tree t) -> [t] -> [t]
insertNodes NilT nodes = nodes
insertNodes (Node v left right) nodes
   | elem v nodes = insertNodes right (insertNodes left nodes)
   | otherwise = insertNodes right (insertNodes left (v:nodes))
   
insertEdges :: Eq t => Tree t -> [(t,t)] -> [(t,t)]
insertEdges NilT edges = edges
insertEdges (Node v left right) edges = (insertEdge v left) ++ (insertEdge v right) ++ (insertEdges left(insertEdges right edges))

insertEdge :: Eq t => t -> Tree t -> [(t,t,)]
insertEdge v NilT = []
insertEdge v (Node v2 left right) = [(v,v2)]