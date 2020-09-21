module View exposing (view)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href, target)
import Maybe.Extra as MaybeE

import Wordlist
import Alphabet exposing (LetterType(..))
import Game exposing (Model, Msg(..), Stage(..))

maybeDrawButtons model = if (shouldDraw model) then drawButtons else emptyDiv 

maybeTimer x = case x.stage of
  Thinking n -> Html.div [class "timer"] [Html.text (String.fromInt n)]
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

wordLinkURL w = "https://naob.no/søk/" ++ (String.toLower w)

shownWord cls w = Html.span [class cls] [Html.a [href (wordLinkURL w), target "_blank"] [Html.text w]]

maybeShownWordView x = case x.stage of
  ShowingWord _ _ s -> Html.div [] [Html.text s]
  ShowedWord s -> case x.wordlist of
    Nothing -> Html.div [] [Html.text s]
    Just wl -> let ok = Wordlist.lookup wl (String.toLower s)
                   cls = if ok then "word-good" else "word-bad"
               in shownWord cls s
  _ -> emptyDiv

maybeNewAnswerButton x = if not (case x.stage of
  ShowingWord _ _ _ -> True
  ShowedWord _ -> True
  _ -> False) then emptyDiv else (Html.div [] [
    Html.button [onClick NewAnswer] [Html.text "Nytt svar"]
  ])

stopTimerButton = Html.div [] [
  Html.button [onClick StopTimer, class "stop-timer"] [Html.text "Gjett nå!"]
  ]

isThinking stage = case stage of
  Thinking _ -> True
  _ -> False

showAnagramsView : List String -> Html Msg
showAnagramsView xs = let f s = Html.div [] [Html.text s]
                      in Html.div [] (List.map f xs)

shouldShowLetters stage = case stage of
  Drawing _ -> True
  Thinking _ -> True
  ShowingSolutions -> True
  _ -> False

canShowSolutions stage = case stage of
  DeclaringLength -> True
  ShowingWord _ _ _ -> True
  ShowedWord _ -> True
  _ -> False

view : Model -> Html Msg
view model =
  Html.div
    []
    [ if shouldShowLetters model.stage
      then lettersView (paddedLetters model)
      else emptyDiv
    , maybeDrawButtons model
    , maybeTimer model
    , if model.stage /= DeclaringLength then emptyDiv else declareLengthButtons (List.length model.letters)
    , if not (isThinking model.stage) then emptyDiv else stopTimerButton
    , maybeShowWordView model
    , maybeShownWordView model
    , Html.div [class "restart-buttons"] [
        maybeNewAnswerButton model
      , if canShowSolutions model.stage
        then Html.button [onClick ShowSolutions]
                         [Html.text "Vis løsninger"]
        else emptyDiv
      , Html.button [onClick NewRound] [Html.text "Ny runde"]
      ]
    , case model.stage of
        ShowingSolutions -> case model.wordlist of 
          Just wl -> let anagrams = Wordlist.partialAnagramsOf wl (String.fromList model.letters)
                     in showAnagramsView (List.take 10 anagrams)
          _ -> emptyDiv
        _ -> emptyDiv
    ]

drawButtons : Html Msg
drawButtons = Html.div [] [
    Html.button [ onClick (Draw Vowel), class "draw" ] [ Html.text "Vokal" ]
  , Html.button [ onClick (Draw Consonant), class "draw" ] [ Html.text "Konsonant" ]
  ]

letterView : Maybe Char -> Html Msg
letterView mch = case mch of
    Just ch -> Html.span [class "letter-view"] [ Html.text (String.fromChar ch) ]
    Nothing -> Html.span [class "letter-view"] [ Html.text "?" ]

padList : Int -> List Char -> List (Maybe Char)
padList n xs = let diff = n - (List.length xs)
                   mxs = List.map Just xs
               in if diff <= 0 then mxs else (mxs ++ List.repeat diff Nothing)

paddedLetters : Model -> List (Maybe Char)
paddedLetters model = padList model.rules.numLetters model.letters

lettersView : List (Maybe Char) -> Html Msg
lettersView xs = Html.div [class "letters-view"] (List.map letterView xs)

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

