module FracTest exposing (suite)

import Expect
import Frac
import Fuzz exposing (Fuzzer, tuple)
import Test exposing (..)


fuzzer : Fuzzer Frac.Frac
fuzzer =
    Fuzz.oneOf [ Fuzz.constant Frac.bottom, Fuzz.constant Frac.top ]
        |> Fuzz.list
        |> Fuzz.map (List.foldl Frac.between Frac.top)


suite : Test
suite =
    fuzzWith { runs = 2 } (tuple ( fuzzer, fuzzer )) "Frac.between return a value between its arguments" <|
        \( a, b ) ->
            let
                expectations =
                    case Frac.compare a b of
                        GT ->
                            [ Frac.compare a >> Expect.equal GT
                            , Frac.compare b >> Expect.equal LT
                            ]

                        LT ->
                            [ Frac.compare a >> Expect.equal LT
                            , Frac.compare b >> Expect.equal GT
                            ]

                        EQ ->
                            [ Frac.compare a >> Expect.equal EQ
                            , Frac.compare b >> Expect.equal EQ
                            ]
            in
            Expect.all expectations (Frac.between a b)
