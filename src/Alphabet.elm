module Alphabet exposing (
  Alphabet,
  LetterType(..),
  letterGenerator,
  norwegianAlphabet)

import Random exposing (Generator)

type LetterType = Vowel | Consonant

type alias WeightedChar = { letter : Char, weight : Float }

charsetTotalWeight : List WeightedChar -> Float
charsetTotalWeight xs = List.foldl (+) 0 (List.map (\x -> x.weight) xs)

charsetSelect w xs = case xs of
  [] -> '?' {- This should never happen ... -}
  (x :: xss) -> if w < x.weight then x.letter else (charsetSelect (w - x.weight) xss)

letterGenerator : List WeightedChar -> Generator Char
letterGenerator cs = Random.map (\w -> charsetSelect w cs) (Random.float 0 (charsetTotalWeight cs))

type alias Alphabet = {
    vowels : List WeightedChar
  , consonants : List WeightedChar
  }

type LetterSpec = Letter Char Float LetterType

letterType (Letter _ _ lt) = lt

fromSpec : (List LetterSpec) -> Alphabet
fromSpec xs = {
    vowels = List.map (\(Letter ch w _) -> { letter = ch, weight = w }) (List.filter (\x -> (letterType x) == Vowel) xs)
  , consonants = List.map (\(Letter ch w _) -> { letter = ch, weight = w }) (List.filter (\x -> (letterType x) == Consonant) xs)
  }

applySpecSmoothing : Float -> List LetterSpec -> List LetterSpec
applySpecSmoothing k xs = List.map (\(Letter ch kk t) -> Letter ch (k + kk) t) xs

smoothingConstant = 0.5

norwegianAlphabet = fromSpec (applySpecSmoothing smoothingConstant [
    Letter 'A' 4.9 Vowel
  , Letter 'B' 1.6 Consonant
  , Letter 'C' 0.15 Consonant
  , Letter 'D' 2.8 Consonant
  , Letter 'E' 11.5 Vowel
  , Letter 'F' 1.7 Consonant
  , Letter 'G' 3.0 Consonant
  , Letter 'H' 1.0 Consonant
  , Letter 'I' 4.7 Vowel
  , Letter 'J' 0.9 Consonant
  , Letter 'K' 2.9 Consonant
  , Letter 'L' 4.6 Consonant
  , Letter 'M' 2.5 Consonant
  , Letter 'N' 5.6 Consonant
  , Letter 'O' 4.1 Vowel
  , Letter 'P' 1.2 Consonant
  , Letter 'Q' 0.0 Consonant
  , Letter 'R' 6.3 Consonant
  , Letter 'S' 5.8 Consonant
  , Letter 'T' 6.5 Consonant
  , Letter 'U' 1.3 Vowel
  , Letter 'V' 1.9 Consonant
  , Letter 'W' 0.0 Consonant
  , Letter 'X' 0.0 Consonant
  , Letter 'Y' 0.4 Vowel
  , Letter 'Z' 0.0 Consonant
  , Letter 'Æ' 0.2 Vowel
  , Letter 'Ø' 0.7 Vowel
  , Letter 'Å' 2.0 Vowel
  ])

