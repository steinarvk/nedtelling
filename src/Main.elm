module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Json.Decode exposing (Value)
import Maybe.Extra as MaybeE
import Time

import Alphabet exposing (Alphabet, LetterType(..))

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

type alias Model = 
  { rules : Rules
  , stage : Stage
  , letters : List Char
  }

startGame : Rules -> Model
startGame ru =
  { rules = ru
  , stage = Drawing ru.numLetters
  , letters = []
  }

initialState = startGame defaultRules

type Msg = Draw LetterType
         | DrewLetter Char
         | ClockTick
         | DeclareLength Int
         | ShowLetter Int
         | NewAnswer
         | NewRound

main : Program Value Model Msg
main = Browser.element
          { init = \_ -> ( initialState, Cmd.none )
          , view = view
          , update = update
          , subscriptions = subscriptions
          }

subscriptions : Model -> Sub Msg
subscriptions model = if (shouldThink model) then (Time.every 1000 (\_ -> ClockTick)) else Sub.none

drawCmd : LetterType -> Alphabet -> Cmd Msg
drawCmd lt al = let f wcs = Random.generate DrewLetter (Alphabet.letterGenerator wcs)
                in case lt of
                  Vowel -> f al.vowels
                  Consonant -> f al.consonants

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

shouldThink : Model -> Bool
shouldThink model = case model.stage of
  Thinking _ -> True
  _ -> False

maybeDrawButtons model = if (shouldDraw model) then drawButtons else emptyDiv 

maybeTimer x = case x.stage of
  Thinking n -> Html.div [] [Html.text (String.fromInt n)]
  _ -> emptyDiv

showWordButton : Int -> Char -> Html Msg
showWordButton i ch = Html.button [onClick (ShowLetter i)] [Html.text (String.fromChar ch)]

showWordButtons : List Char -> Html Msg
showWordButtons chs = Html.div [] (List.indexedMap showWordButton chs)

maybeShowWordView x = case x.stage of
  ShowingWord _ left s -> Html.div [] [
      Html.div [] [ showWordButtons left ]
    ]
  _ -> emptyDiv

maybeShownWordView x = case x.stage of
  ShowingWord _ _ s -> Html.div [] [Html.text s]
  ShowedWord s -> Html.div [] [Html.text s]
  _ -> emptyDiv

maybeNewAnswerButton x = if not (case x.stage of
  ShowingWord _ _ _ -> True
  ShowedWord _ -> True
  _ -> False) then emptyDiv else (Html.div [] [
    Html.button [onClick NewAnswer] [Html.text "Nytt svar"]
  ])

view : Model -> Html Msg
view model =
  Html.div
    []
    [ case model.stage of
        Drawing _ -> lettersView (paddedLetters model)
        Thinking _ -> lettersView (paddedLetters model)
        _ -> emptyDiv
    , maybeDrawButtons model
    , maybeTimer model
    , if model.stage /= DeclaringLength then emptyDiv else declareLengthButtons (List.length model.letters)
    , maybeShowWordView model
    , maybeShownWordView model
    , maybeNewAnswerButton model
    , Html.button [onClick NewRound] [Html.text "Ny runde"]
    ]

drawButtons : Html Msg
drawButtons = Html.div [] [
    Html.button [ onClick (Draw Vowel) ] [ Html.text "Vokal" ]
  , Html.button [ onClick (Draw Consonant) ] [ Html.text "Konsonant" ]
  ]

letterView : Maybe Char -> Html Msg
letterView mch = case mch of
    Just ch -> Html.button [] [ Html.text (String.fromChar ch) ]
    Nothing -> Html.button [] [ Html.text "?" ]

padList : Int -> List Char -> List (Maybe Char)
padList n xs = let diff = n - (List.length xs)
                   mxs = List.map Just xs
               in if diff <= 0 then mxs else (mxs ++ List.repeat diff Nothing)

paddedLetters : Model -> List (Maybe Char)
paddedLetters model = padList model.rules.numLetters model.letters

lettersView : List (Maybe Char) -> Html Msg
lettersView xs = Html.div [] (List.map letterView xs)

declareLengthButton : Int -> Html Msg
declareLengthButton n = Html.button [ onClick (DeclareLength n) ] [ Html.text (String.fromInt n) ]

declareLengthButtons : Int -> Html Msg
declareLengthButtons n = Html.div [] ([
  Html.div [] [ Html.text "Hvor mange bokstaver er ordet ditt?" ]
  ] ++ (List.map declareLengthButton (List.range 1 n)))

emptyDiv = Html.div [] []

shouldDraw : Model -> Bool
shouldDraw model = case model.stage of
  Drawing _ -> True
  _ -> False

