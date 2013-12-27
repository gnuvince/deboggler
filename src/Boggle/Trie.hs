module Boggle.Trie ( Trie
                   , empty
                   , add
                   , contains
                   , fromList
                   )
where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')

data Trie = Trie Bool (M.Map Char Trie)
          deriving (Show, Eq)

empty :: Trie
empty = Trie False M.empty

add :: B.ByteString -> Trie -> Trie
add word (Trie eow m) =
    case B.uncons word of
      Nothing -> Trie True m
      Just (c, cs) ->
        case M.lookup c m of
          Nothing -> Trie eow (M.insert c (add cs empty) m)
          Just t  -> Trie eow (M.insert c (add cs t) m)

contains :: B.ByteString -> Trie -> Bool
contains word (Trie eow m) =
    case B.uncons word of
      Nothing -> eow
      Just (c, cs) ->
          case M.lookup c m of
            Nothing -> False
            Just t  -> contains cs t

fromList :: [B.ByteString] -> Trie
fromList words = foldl' (\t w -> add w t) empty words
