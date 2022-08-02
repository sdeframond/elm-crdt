module LWWRegTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, constant, int, oneOf, tuple)
import Helpers
import LWWReg
import Test exposing (..)


replica : List String -> Fuzzer String
replica rids =
    rids |> List.map constant |> oneOf


fuzzer : List String -> Fuzzer (LWWReg.LWWReg ( Int, String ) Int)
fuzzer rids =
    Fuzz.map2 LWWReg.init (tuple ( int, replica rids )) int


nextTick ( i, rid ) =
    ( i + 1, rid )


suite : Test
suite =
    describe "LWWReg"
        [ Helpers.itIsACrdt
            { merge = LWWReg.merge
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            }
        , Helpers.itIsValueDiffable
            { fuzzer = fuzzer [ "A", "B" ]
            , makeDiff = LWWReg.makeDiff (-)
            , apply = (+) >> LWWReg.update nextTick ( 0, "" )
            , value = LWWReg.value
            }
        , fuzz int ".value returns the right value" <|
            \i ->
                LWWReg.init ( 0, "A" ) i |> LWWReg.value |> Expect.equal i
        , fuzz2 (tuple ( int, replica [ "A", "B" ] )) (tuple ( int, replica [ "A", "B" ] )) "merge keeps the value with the greatest timestamp" <|
            \ts1 ts2 ->
                LWWReg.merge
                    (LWWReg.init ts1 ts1)
                    (LWWReg.init ts2 ts2)
                    |> LWWReg.value
                    |> Expect.equal (max ts1 ts2)
        , test "causality is preserved even with stale timestamps" <|
            \_ ->
                let
                    reg =
                        LWWReg.init 2 0

                    updated =
                        LWWReg.update ((+) 1) 1 ((+) 1) reg
                in
                LWWReg.merge reg updated |> LWWReg.value |> Expect.equal 1
        ]
