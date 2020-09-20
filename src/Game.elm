module Game exposing (Model, Rules, Msg(..), Stage(..), startGame, update, defaultRules)

import Alphabet exposing (Alphabet, LetterType(..))

import Random

type Stage = Drawing Int | Thinking Int | DeclaringLength | ShowingWord Int (List Char) String | ShowedWord String

type alias Rules =
  { alphabet : Alphabet
  , numLetters : Int
  , thinkingTimeSeconds : Int
  }

defaultRules =
  { alphabet = Alphabet.norwegianAlphabet
  , numLetters = 9
  , thinkingTimeSeconds = 60
  }

startGame : Rules -> Model
startGame ru =
  { rules = ru
  , stage = Drawing ru.numLetters
  , letters = []
  }

type alias Model = 
  { rules : Rules
  , stage : Stage
  , letters : List Char
  }

type Msg = Draw LetterType
         | DrewLetter Char
         | ClockTick
         | DeclareLength Int
         | ShowLetter Int
         | StopTimer
         | NewAnswer
         | NewRound

beginCountdown : Model -> Model
beginCountdown m = { m | stage = Thinking m.rules.thinkingTimeSeconds }

tickSecond : Model -> Model
tickSecond m = case m.stage of
  Thinking 1 -> { m | stage = DeclaringLength }
  Thinking n -> { m | stage = Thinking (n-1) }
  _ -> m

addLetter : Char -> Model -> Model
addLetter ch model = case model.stage of
  Drawing n -> let nm = { model | letters = model.letters ++ [ch] }
               in if n == 1 then beginCountdown nm else { nm | stage = Drawing (n-1) }
  _ -> model

removeAt i xs = List.map (\(_,x) -> x) (List.filter (\(j,_) -> j /= i) (List.indexedMap Tuple.pair xs))

getAt i xs = case List.head (List.filter (\(j,_) -> j == i) (List.indexedMap Tuple.pair xs)) of
  Nothing -> '!'
  Just (_, ch) -> ch

updateShownWord : Int -> Model -> Model
updateShownWord i m = case m.stage of
  ShowingWord n xs s ->
      let ns = (s ++ (String.fromChar (getAt i xs)))
          nl = (removeAt i xs)
          nn = n - 1
      in if nn == 0
         then { m | stage = ShowedWord ns }
         else { m | stage = ShowingWord nn nl ns }
  _ -> m

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Draw lt -> (model, drawCmd lt model.rules.alphabet)
  DrewLetter ch -> (addLetter ch model, Cmd.none)
  ClockTick -> (tickSecond model, Cmd.none)
  DeclareLength n -> ({ model | stage = ShowingWord n model.letters "" }, Cmd.none)
  ShowLetter index -> (updateShownWord index model, Cmd.none)
  NewAnswer -> ({ model | stage = DeclaringLength }, Cmd.none)
  NewRound -> (startGame model.rules, Cmd.none)
  StopTimer -> ({ model | stage = DeclaringLength }, Cmd.none)

drawCmd : LetterType -> Alphabet -> Cmd Msg
drawCmd lt al = let f wcs = Random.generate DrewLetter (Alphabet.letterGenerator wcs)
                in case lt of
                  Vowel -> f al.vowels
                  Consonant -> f al.consonants
