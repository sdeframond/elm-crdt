module PNCounterTest exposing (fuzzer, operation, suite)

import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, tuple)
import Helpers exposing (itIsAnAnonymousCrdt, itIsAnonymouslyDiffable, itIsUndoable)
import PNCounter exposing (Operation(..))
import Test exposing (..)


applyOps : List ( String, Operation ) -> PNCounter.PNCounter -> PNCounter.PNCounter
applyOps ops counter =
    case ops of
        [] ->
            counter

        ( rid, op ) :: tail ->
            PNCounter.apply rid op counter
                |> applyOps tail


operation : Fuzzer Operation
operation =
    [ Inc, Dec ] |> List.map constant |> oneOf


actionsFuzzer : Fuzzer (List ( String, Operation ))
actionsFuzzer =
    let
        replicas =
            [ "A", "B", "C", "D" ]
    in
    tuple
        ( replicas |> List.map constant |> oneOf
        , operation
        )
        |> list


fuzzer : Fuzzer PNCounter.PNCounter
fuzzer =
    Fuzz.map (\ops -> applyOps ops PNCounter.init) actionsFuzzer


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
        , itIsUndoable
            { apply = PNCounter.apply "A"
            , unapply = PNCounter.unapply "A"
            , value = PNCounter.value
            , fuzzData = fuzzer
            , fuzzOpMaker = operation |> Fuzz.map (\o -> always o)
            }
        , fuzz actionsFuzzer "it counts alright" <|
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
