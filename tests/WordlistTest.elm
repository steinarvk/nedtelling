module WordlistTest exposing (..)

import Wordlist

import Expect exposing (Expectation)
import Test exposing (..)

wl = Wordlist.fromList [
    "i"
  , "ikke"
  , "ikkje"
  , "illebefinnende"
  ]

shouldContainTests =
    describe "Example word list should contain" (
      let maketest w = test ("contains: " ++ w) (\_ -> Expect.equal True (Wordlist.lookup wl w))
      in List.map maketest ["ikke", "ikkje", "i", "illebefinnende"])

shouldNotContainTests =
    describe "Example word list should NOT contain" (
      let maketest w = test ("does NOT contain: " ++ w) (\_ -> Expect.equal False (Wordlist.lookup wl w))
      in List.map maketest ["ille", "ikk", "j"])
