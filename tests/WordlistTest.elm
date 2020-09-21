module WordlistTest exposing (..)

import Wordlist

import Expect exposing (Expectation)
import Test exposing (..)

wl = Wordlist.fromList [
    "i"
  , "ikke"
  , "ikkje"
  , "illebefinnende"
  , "lose"
  , "sole"
  , "stole"
  ]

shouldContainTests =
    describe "Example word list should contain" (
      let maketest w = test ("contains: " ++ w) (\_ -> Expect.equal True (Wordlist.lookup wl w))
      in List.map maketest ["ikke", "ikkje", "i", "illebefinnende"])

shouldNotContainTests =
    describe "Example word list should NOT contain" (
      let maketest w = test ("does NOT contain: " ++ w) (\_ -> Expect.equal False (Wordlist.lookup wl w))
      in List.map maketest ["ille", "ikk", "j"])

anagramTests =
    describe "Anagram lists" [
        test "sloe" (\_ -> Expect.equal (Wordlist.anagramsOf wl "sloe") ["lose", "sole"])
      , test "olse" (\_ -> Expect.equal (Wordlist.anagramsOf wl "olse") ["lose", "sole"])
      , test "ekki" (\_ -> Expect.equal (Wordlist.anagramsOf wl "ekki") ["ikke"])
      , test "ekkij" (\_ -> Expect.equal (Wordlist.anagramsOf wl "ekkij") ["ikkje"])
      , test "lxxzq" (\_ -> Expect.equal (Wordlist.anagramsOf wl "lxxzq") [])
      ]
