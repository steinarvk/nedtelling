module Main exposing (main)

import Browser
import Json.Decode exposing (Value)
import Time

import Game exposing (Msg(..), Stage(..))
import View

main : Program Value Game.Model Game.Msg
main = Browser.element
          { init = \_ -> ( Game.startGame Game.defaultRules, Cmd.none )
          , view = View.view
          , update = Game.update
          , subscriptions = subscriptions
          }

subscriptions : Game.Model -> Sub Game.Msg
subscriptions model = if (clockIsTicking model)
                      then (Time.every 1000 (\_ -> ClockTick))
                      else Sub.none

clockIsTicking : Game.Model -> Bool
clockIsTicking model = case model.stage of
  Thinking _ -> True
  _ -> False
