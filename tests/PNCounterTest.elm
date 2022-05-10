module PNCounterTest exposing (..)

import CrdtHelper exposing (itIsACrdt)
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, tuple)
import PNCounter
import Test exposing (..)


type Operation
    = Inc
    | Dec


applyOps : List ( String, Operation ) -> PNCounter.PNCounter -> PNCounter.PNCounter
applyOps ops counter =
    let
        apply rid op c =
            case op of
                Inc ->
                    PNCounter.increment rid c

                Dec ->
                    PNCounter.decrement rid c
    in
    case ops of
        [] ->
            counter

        ( rid, op ) :: tail ->
            apply rid op counter
                |> applyOps tail


operationsFuzzer : Fuzzer (List ( String, Operation ))
operationsFuzzer =
    let
        replicas =
            [ "A", "B", "C", "D" ]

        operations =
            [ Inc, Dec ]
    in
    tuple
        ( replicas |> List.map constant |> oneOf
        , operations |> List.map constant |> oneOf
        )
        |> list


pNCounterFuzzer : Fuzzer PNCounter.PNCounter
pNCounterFuzzer =
    Fuzz.map (\ops -> applyOps ops PNCounter.init) operationsFuzzer


suite : Test
suite =
    describe "PNCounter"
        [ itIsACrdt { fuzzer = pNCounterFuzzer, merge = PNCounter.merge }
        , fuzz operationsFuzzer "it counts alright" <|
            \ops ->
                let
                    count =
                        List.partition (Tuple.second >> (==) Inc) ops
                            |> (\( incs, decs ) -> List.length incs - List.length decs)
                in
                applyOps ops PNCounter.init
                    |> PNCounter.value
                    |> Expect.equal count
        ]
