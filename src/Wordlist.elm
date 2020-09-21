module Wordlist exposing (Wordlist, fromList, lookup, anagramsOf, fromFrequencyListString, partialAnagramsOf)

import Set exposing (Set)
import Dict exposing (Dict)

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
anagramsOf (Wordlist _ d) s = anagramDictLookup d (String.toLower s)

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

powerset : List a -> List (List a)
powerset xs = case xs of
  [] -> [[]]
  (first :: rest) -> let prest = powerset rest
                     in (List.map (\option -> [first] ++ option) prest) ++ prest

stringPowerset s = List.map String.fromList (powerset (String.toList s))

partialAnagramsOf : Wordlist -> String -> List String
partialAnagramsOf wl s =
  let raw = List.concatMap (anagramsOf wl) (List.filter (\x -> x /= "") (stringPowerset s))
      uniq = Set.toList (Set.fromList raw)
      byLength = List.reverse (List.sortBy String.length uniq)
  in byLength

anagramDictLookup : Dict String (List String) -> String -> List String
anagramDictLookup d s =
  case Dict.get (canonicalizeAnagram s) d of
    Nothing -> []
    Just options -> options
