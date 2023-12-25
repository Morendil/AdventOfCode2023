import Data.Graph
import Data.Maybe (fromJust)
import Data.List (nub, (\\))


toEdge ws = (head ws, head ws, tail ws)
ffst (f,_,_) = f

main = do
    arrows <- map words . lines <$> readFile "day25_minimal.txt"
    let (graph, vToE, keyToV) = graphFromEdges $ map toEdge arrows
        verts = vertices graph
        bounds = (minimum verts, maximum verts)
        undirected = buildG bounds (edges graph ++ edges (transposeG graph))
    print $ length $ reachable undirected (fromJust $ keyToV "fjt")
    print $ length $ reachable undirected (fromJust $ keyToV "jbj")
    let left = map (ffst . vToE) $ reachable undirected (fromJust $ keyToV "vpd")
        right = map (ffst . vToE) $ reachable undirected (fromJust $ keyToV "jbj")
    print $ (nub $ concat arrows) \\ (left ++ right)