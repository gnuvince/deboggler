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


-- |Given a set of words, a board specification and a list of paths,
-- return a set of all the valid words (i.e. words in the dictionary).
findWords :: Dict -> B.ByteString -> [[Int]] -> S.Set B.ByteString
findWords dict board paths =
    S.fromList [ word
               | path <- paths
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
  let paths = concat $
                     parMap rdeepseq (\(n,d) -> G.dfs d n graph)
                     [(n, d) | n <- [0..15], d <- [3..10]]
  let words = findWords dict board paths
  mapM_ B.putStrLn (sortBy (compare `on` B.length) (S.toList words))
