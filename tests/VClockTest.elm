module VClockTest exposing (suite)

import CrdtHelper exposing (isASimpleCrdt)
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf)
import Test exposing (..)
import VClock


fromList : List String -> VClock.VClock -> VClock.VClock
fromList rids counter =
    case rids of
        [] ->
            counter

        rid :: tail ->
            VClock.increment rid counter
                |> fromList tail


operationsFuzzer : Fuzzer (List String)
operationsFuzzer =
    let
        replicas =
            [ "A", "B", "C", "D" ]
    in
    list (replicas |> List.map constant |> oneOf)


fuzzer : Fuzzer VClock.VClock
fuzzer =
    Fuzz.map (\rids -> fromList rids VClock.zero) operationsFuzzer


suite : Test
suite =
    describe "VTime"
        [ isASimpleCrdt { fuzzer = fuzzer, merge = VClock.merge }
        , testCompare
        ]


testCompare : Test
testCompare =
    describe "VClock.compare"
        [ fuzz fuzzer "Equal" <|
            \vcl ->
                VClock.compare vcl vcl
                    |> Expect.equal VClock.EQ
        , fuzz fuzzer "Lower than" <|
            \vcl ->
                VClock.compare vcl (VClock.increment "A" vcl)
                    |> Expect.equal VClock.LT
        , fuzz fuzzer "Greater than" <|
            \vcl ->
                VClock.compare (VClock.increment "A" vcl) vcl
                    |> Expect.equal VClock.GT
        , fuzz fuzzer "Can't compare" <|
            \vcl ->
                VClock.compare
                    (VClock.increment "A" vcl)
                    (VClock.increment "B" vcl)
                    |> Expect.equal VClock.CC
        ]
