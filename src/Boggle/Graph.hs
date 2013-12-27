module Boggle.Graph ( Graph
                    , empty
                    , neighbors
                    , addEdge
                    , boggleGraph
                    , dfs
                    )
where

import qualified Data.IntMap as M
import qualified Data.Set    as S
import qualified Data.List   as L

type Cube = Int

newtype Graph = MkGraph (M.IntMap (S.Set Cube))
    deriving (Show, Eq)

empty :: Graph
empty = MkGraph (M.empty)

neighbors :: Cube -> Graph -> S.Set Cube
neighbors node (MkGraph m) =
    M.findWithDefault S.empty node m

addEdge :: Cube -> Cube -> Graph -> Graph
addEdge x y (MkGraph m) =
    MkGraph $ M.insertWith S.union x (S.singleton y)
            $ M.insertWith S.union y (S.singleton x) m

boggleGraph :: Int -> Graph
boggleGraph size = L.foldl' addCube empty [0 .. size^2 - 1]
    where
        addCube :: Graph -> Cube -> Graph
        addCube graph cube =
            L.foldl' (\g (di, dj) -> if inBoard (i+di, j+dj) then
                                         addEdge cube (from2D (i+di, j+dj)) g
                                     else
                                         g) graph deltas
            where deltas = [(-1, -1), (-1,  0), (-1,  1),
                            ( 0, -1),           ( 0,  1),
                            ( 1, -1), ( 1,  0), ( 1,  1)]
                  (i, j) = to2D cube
                  inBoard (x, y) = x >= 0 && x <= size-1 && y >= 0 && y <= size-1

        to2D :: Int -> (Int, Int)
        to2D n = n `quotRem` size

        from2D :: (Int, Int) -> Int
        from2D (i, j) = size*i + j

dfs :: Int -> Int -> Graph -> [[Cube]]
dfs depth cube g = go depth cube S.empty
    where go 0 _ _    = []
          go 1 curr _ = [[curr]]
          go d curr visited = map (curr:) $ concat [go (d-1) adj (S.insert curr visited)
                                                    | adj <- S.toList (neighbors curr g)
                                                    , adj `S.notMember` visited]
