module GSetTest exposing (..)

import CrdtHelper exposing (isASimpleCrdt)
import Expect
import Fuzz exposing (Fuzzer, list, string)
import GSet
import Set
import Test exposing (..)


fromList : List comparable -> GSet.GSet comparable
fromList =
    List.foldl GSet.insert GSet.empty


gSetFuzzer : Fuzzer (GSet.GSet String)
gSetFuzzer =
    list string
        |> Fuzz.map fromList


suite : Test
suite =
    describe "GSet"
        [ isASimpleCrdt { fuzzer = gSetFuzzer, merge = GSet.merge }
        , fuzz (list string) "it keeps the inserted items" <|
            \l -> fromList l |> GSet.toSet |> Expect.equal (Set.fromList l)
        ]


memberTest : Test
memberTest =
    describe ".member"
        [ test "returns false when the value has not been inserted" <|
            \_ ->
                GSet.empty
                    |> GSet.member "foo"
                    |> Expect.equal False
        , test "return true when the value has been inserted" <|
            \_ ->
                GSet.empty
                    |> GSet.insert "foo"
                    |> GSet.member "foo"
                    |> Expect.equal True
        ]
