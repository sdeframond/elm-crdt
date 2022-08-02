module LWWRegTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, constant, int, oneOf, tuple)
import Helpers
import LWWReg
import Test exposing (..)


replica : List String -> Fuzzer String
replica rids =
    rids |> List.map constant |> oneOf


fuzzer : List String -> Fuzzer (LWWReg.LWWReg Int)
fuzzer rids =
    Fuzz.map2 LWWReg.init (tuple ( int, replica rids )) int


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
            , apply = (+) >> LWWReg.update ( 0, "A" )
            , value = LWWReg.value
            }
        , fuzz int ".value returns the right value" <|
            \i ->
                LWWReg.init ( 0, "A" ) i |> LWWReg.value |> Expect.equal i
        ]
