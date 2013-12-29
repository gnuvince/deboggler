module Boggle.Trie ( Trie
                   , empty
                   , add
                   , contains
                   , fromList
                   )
where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import           Data.List (foldl')
import           Prelude hiding (words)
import           Data.Maybe (fromMaybe)

data Trie = MkTrie !Bool (M.Map Char Trie)
          deriving (Show, Eq)

-- |Create an empty trie.
empty :: Trie
empty = MkTrie False M.empty

-- |Add a word to a trie.
add :: B.ByteString -> Trie -> Trie
add word (MkTrie eow m) =
    case B.uncons word of
      Nothing -> MkTrie True m
      Just (c, cs) ->
          case M.lookup c m of
            Nothing -> MkTrie eow (M.insert c (add cs empty) m)
            Just t  -> MkTrie eow (M.insert c (add cs t) m)

-- |Check if a word is in the trie.
contains :: B.ByteString -> Trie -> Bool
contains word (MkTrie eow m) =
    case B.uncons word of
      Nothing -> eow
      Just (c, cs) ->
          case M.lookup c m of
            Nothing -> False
            Just t  -> contains cs t

-- |Add all the @words@ to an empty trie.
fromList :: [B.ByteString] -> Trie
fromList words = foldl' (\t w -> add w t) empty words
