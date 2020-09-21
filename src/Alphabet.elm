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
    Letter 'A' 5.8 Vowel
  , Letter 'B' 1.6 Consonant
  , Letter 'C' 0.08 Consonant
  , Letter 'D' 4.3 Consonant
  , Letter 'E' 15.9 Vowel
  , Letter 'F' 2.2 Consonant
  , Letter 'G' 3.8 Consonant
  , Letter 'H' 1.2 Consonant
  , Letter 'I' 6.3 Vowel
  , Letter 'J' 0.7 Consonant
  , Letter 'K' 3.8 Consonant
  , Letter 'L' 5.3 Consonant
  , Letter 'M' 3.4 Consonant
  , Letter 'N' 7.7 Consonant
  , Letter 'O' 5.3 Vowel
  , Letter 'P' 1.9 Consonant
  , Letter 'Q' 0.01 Consonant
  , Letter 'R' 8.8 Consonant
  , Letter 'S' 6.1 Consonant
  , Letter 'T' 8.3 Consonant
  , Letter 'U' 1.6 Vowel
  , Letter 'V' 2.6 Consonant
  , Letter 'W' 0.03 Consonant
  , Letter 'X' 0.008 Consonant
  , Letter 'Y' 0.7 Vowel
  , Letter 'Z' 0.008 Consonant
  , Letter 'Æ' 0.2 Vowel
  , Letter 'Ø' 0.8 Vowel
  , Letter 'Å' 1.4 Vowel
  ])

