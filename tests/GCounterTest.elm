module GCounterTest exposing (..)

import CrdtHelper exposing (itIsACrdt)
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf)
import GCounter
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
        [ itIsACrdt { fuzzer = gCounterFuzzer, merge = GCounter.merge }
        , fuzz operationsFuzzer "it gives the correct value" <|
            \l ->
                fromList l GCounter.zero
                    |> GCounter.value
                    |> Expect.equal (List.length l)
        ]
