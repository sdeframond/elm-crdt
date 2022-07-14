module Helpers exposing (isAnAnonymousCrdt, itIsACrdt)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


isAnAnonymousCrdt : { fuzzer : Fuzzer a, merge : a -> a -> a } -> Test
isAnAnonymousCrdt { fuzzer, merge } =
    itIsACrdt
        { fuzzerA = fuzzer
        , fuzzerB = fuzzer
        , fuzzerC = fuzzer
        , merge = merge
        }


itIsACrdt : { fuzzerA : Fuzzer a, fuzzerB : Fuzzer a, fuzzerC : Fuzzer a, merge : a -> a -> a } -> Test
itIsACrdt { fuzzerA, fuzzerB, fuzzerC, merge } =
    describe "it is a CRDT"
        [ fuzz fuzzerA "it is idempotent" <|
            \crdt -> merge crdt crdt |> Expect.equal crdt
        , fuzz2 fuzzerA fuzzerB "it is commutative" <|
            \a b -> merge a b |> Expect.equal (merge b a)
        , fuzz3 fuzzerA fuzzerB fuzzerC "it is associative" <|
            \a b c ->
                merge a (merge b c)
                    |> Expect.equal (merge a b |> merge c)
        ]
