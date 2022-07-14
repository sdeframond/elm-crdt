module GCounterTest exposing (gCounterFuzzer, suite)

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


gCounterFuzzer : Fuzzer GCounter.GCounter
gCounterFuzzer =
    Fuzz.map (\rids -> fromList rids GCounter.zero) operationsFuzzer


suite : Test
suite =
    describe "GCounter"
        [ itIsAnAnonymousCrdt { fuzzer = gCounterFuzzer, merge = GCounter.merge }
        , itIsAnonymouslyDiffable { init = GCounter.zero, merge = GCounter.merge, fuzzer = gCounterFuzzer, delta = GCounter.delta }
        , fuzz operationsFuzzer "it gives the correct value" <|
            \l ->
                fromList l GCounter.zero
                    |> GCounter.value
                    |> Expect.equal (List.length l)
        ]
