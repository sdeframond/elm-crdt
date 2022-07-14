module GCounterTest exposing (fuzzer, suite)

import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf)
import GCounter
import Helpers exposing (itIsAnAnonymousCrdt, itIsAnonymouslyDiffable)
import Test exposing (..)


fromList : List String -> GCounter.GCounter -> GCounter.GCounter
fromList rids counter =
    case rids of
        [] ->
            counter

        rid :: tail ->
            GCounter.increment rid counter
                |> fromList tail


operationsFuzzer : Fuzzer (List String)
operationsFuzzer =
    let
        replicas =
            [ "A", "B", "C", "D" ]
    in
    list (replicas |> List.map constant |> oneOf)


fuzzer : Fuzzer GCounter.GCounter
fuzzer =
    Fuzz.map (\rids -> fromList rids GCounter.init) operationsFuzzer


suite : Test
suite =
    describe "GCounter"
        [ itIsAnAnonymousCrdt { fuzzer = fuzzer, merge = GCounter.merge }
        , itIsAnonymouslyDiffable
            { init = GCounter.init
            , merge = GCounter.merge
            , fuzzer = fuzzer
            , delta = GCounter.delta
            }
        , fuzz operationsFuzzer "it gives the correct value" <|
            \l ->
                fromList l GCounter.init
                    |> GCounter.value
                    |> Expect.equal (List.length l)
        ]
