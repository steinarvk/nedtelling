module Wordlist exposing (Wordlist, fromList, lookup)

import Set exposing (Set)
import Dict exposing (Dict)

type Trie = Trie (Dict Char Trie)

trieLookup : List Char -> Trie -> Bool
trieLookup l (Trie d) = case l of
  (x::xs) -> case Dict.get x d of
    Nothing -> False
    Just st -> trieLookup xs st
  [] -> True

convertWord : String -> List Char
convertWord w = (String.toList w) ++ [terminator]

trieAdd : Trie -> List Char -> Trie
trieAdd (Trie d) l = case l of
  [] -> Trie d
  (x::xs) -> let update mt = Just (case mt of
                                     Just t -> trieAdd t xs
                                     Nothing -> trieAdd emptyTrie xs)
             in Trie (Dict.update x update d)

trieAddWord : String -> Trie -> Trie
trieAddWord s t = trieAdd t (convertWord s)

emptyTrie = Trie Dict.empty

terminator = '.'

type Wordlist = Wordlist (Set String)

fromList : List String -> Wordlist
fromList ws = Wordlist (Set.fromList ws)

lookup : Wordlist -> String -> Bool
lookup (Wordlist ss) s = Set.member s ss

