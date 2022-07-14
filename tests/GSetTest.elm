module GSetTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, list, string)
import GSet
import Helpers exposing (itIsAnAnonymousCrdt, itIsAnonymouslyDiffable)
import Set
import Test exposing (..)


fromList : List comparable -> GSet.GSet comparable
fromList =
    List.foldl GSet.insert GSet.init


fuzzer : Fuzzer (GSet.GSet String)
fuzzer =
    list string
        |> Fuzz.map fromList


suite : Test
suite =
    describe "GSet"
        [ itIsAnAnonymousCrdt { fuzzer = fuzzer, merge = GSet.merge }
        , itIsAnonymouslyDiffable
            { init = GSet.init
            , fuzzer = fuzzer
            , delta = GSet.delta
            , merge = GSet.merge
            }
        , fuzz (list string) "it keeps the inserted items" <|
            \l -> fromList l |> GSet.toSet |> Expect.equal (Set.fromList l)
        , memberTest
        ]


memberTest : Test
memberTest =
    describe ".member"
        [ test "returns false when the value has not been inserted" <|
            \_ ->
                GSet.init
                    |> GSet.member "foo"
                    |> Expect.equal False
        , test "return true when the value has been inserted" <|
            \_ ->
                GSet.init
                    |> GSet.insert "foo"
                    |> GSet.member "foo"
                    |> Expect.equal True
        ]
