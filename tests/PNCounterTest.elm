module PNCounterTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, tuple)
import Helpers exposing (itIsAnAnonymousCrdt, itIsAnonymouslyDiffable)
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


fuzzer : Fuzzer PNCounter.PNCounter
fuzzer =
    Fuzz.map (\ops -> applyOps ops PNCounter.init) operationsFuzzer


suite : Test
suite =
    describe "PNCounter"
        [ itIsAnAnonymousCrdt { fuzzer = fuzzer, merge = PNCounter.merge }
        , itIsAnonymouslyDiffable
            { init = PNCounter.init
            , merge = PNCounter.merge
            , delta = PNCounter.delta
            , fuzzer = fuzzer
            }
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
