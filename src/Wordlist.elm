module Wordlist exposing (Wordlist, fromList, lookup, anagramsOf, fromFrequencyListString)

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

type Wordlist = Wordlist (Set String) (Dict String (List String))

fromList : List String -> Wordlist
fromList ws = Wordlist (Set.fromList ws) (anagramDictFromList ws)

fromFrequencyListString : String -> Wordlist
fromFrequencyListString s =
  let firstColumn line = List.head (String.words line) 
  in fromList (List.filterMap firstColumn (String.lines s))

lookup : Wordlist -> String -> Bool
lookup (Wordlist ss _) s = Set.member s ss

anagramsOf : Wordlist -> String -> List String
anagramsOf (Wordlist _ d) s = anagramDictLookup d s

canonicalizeAnagram : String -> String
canonicalizeAnagram s = (String.fromList (List.sort (String.toList s)))


anagramDictInsert : String -> Dict String (List String) -> Dict String (List String)
anagramDictInsert s d =
  let update ml = case ml of
        Nothing -> Just [s]
        Just other -> Just (List.sort (other ++ [s]))
  in Dict.update (canonicalizeAnagram s) update d

anagramDictFromList : List String -> (Dict String (List String))
anagramDictFromList xs = List.foldl anagramDictInsert Dict.empty xs

anagramDictLookup : Dict String (List String) -> String -> List String
anagramDictLookup d s =
  case Dict.get (canonicalizeAnagram s) d of
    Nothing -> []
    Just options -> options
