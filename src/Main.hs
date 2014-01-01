module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import           System.Environment (getArgs)
import           Data.List (sortBy)
import           Data.Function (on)
import           Prelude hiding (words)
import           Control.Parallel.Strategies

import qualified Boggle.Graph as G
import qualified Boggle.Trie  as T

type Dict = T.Trie

-- |Convert a path (list of indexes) into a word according to a given
-- board specification.  The indexes go from 0 to 15 (inclusively)
-- and refer to a specific cube of the Boggle board.
pathToWord :: B.ByteString -> [Int] -> B.ByteString
pathToWord board path = B.pack [board `B.index` x | x <- path]

process :: Dict -> B.ByteString -> G.Graph -> (Int, Int) -> [B.ByteString]
process dict board graph (n, d) =
        [ word |
          path <- G.dfs d n graph
        , let word = pathToWord board path
        , T.contains word dict
        ]

main :: IO ()
main = do
  (dictFile:boardSpec:_) <- getArgs
  let board = B.pack boardSpec
  content <- B.readFile dictFile
  let dict = T.fromList (B.lines content)
  let graph = G.boggleGraph 4
  let words = S.fromList $ concat $ parMap rdeepseq (process dict board graph)
                                           [(n, d) | n <- [0..15], d <- [3..10]]
  mapM_ B.putStrLn (sortBy (compare `on` B.length) (S.toList words))
