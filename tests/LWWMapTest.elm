module LWWMapTest exposing (suite)

import Dict
import Expect
import Fuzz exposing (Fuzzer, constant, int, list, oneOf, string)
import Helpers
import LWWMap exposing (LWWMap)
import Test exposing (..)


replica : List String -> Fuzzer String
replica rids =
    List.map constant rids
        |> oneOf


operation : List String -> Fuzzer (LWWMap Int String Int -> LWWMap Int String Int)
operation rids =
    oneOf
        [ Fuzz.map4 LWWMap.insert int (replica rids) string int
        , Fuzz.map2 LWWMap.remove (replica rids) string
        ]


fuzzer : List String -> Fuzzer (LWWMap Int String Int)
fuzzer rids =
    let
        applyOperations operations =
            List.foldl (\f map -> f map) LWWMap.init operations
    in
    Fuzz.map applyOperations (list (operation rids))


suite : Test
suite =
    describe "LWWMap"
        [ Helpers.itIsACrdt
            { fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            , merge = LWWMap.merge
            }
        , Helpers.itIsADelta
            { init = LWWMap.init
            , merge = LWWMap.merge
            , delta = LWWMap.delta
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B", "C" ]
            }
        , Helpers.itIsValueDiffable
            { fuzzer = fuzzer [ "A", "B" ]
            , makeDiff = LWWMap.makeDiff (-)
            , apply = LWWMap.applyDiff ((+) 1) 0 "A" (+)
            , value = LWWMap.toDict
            }
        , fuzz2 (fuzzer [ "A", "B" ])
            int
            "Add wins over a concurrent remove"
          <|
            \map value ->
                LWWMap.merge
                    (LWWMap.remove "A" "" map)
                    (LWWMap.remove "B" "" map |> LWWMap.insert 0 "B" "" value)
                    |> LWWMap.member ""
                    |> Expect.true "Expect the empty string to be in the map"
        , test ".insert puts an item into the map" <|
            \_ ->
                LWWMap.init
                    |> LWWMap.insert 0 "A" "foo" "bar"
                    |> LWWMap.get "foo"
                    |> Expect.equal (Just "bar")
        , fuzz2 (fuzzer [ "A", "B" ]) int ".insert is idempotent" <|
            \map value ->
                let
                    a =
                        LWWMap.insert 0 "A" "" value map
                in
                Expect.equal a (LWWMap.insert 0 "A" "" value a)
        , fuzz (fuzzer [ "A", "B" ]) ".remove is idempotent" <|
            \map ->
                let
                    a =
                        LWWMap.remove "A" "" map
                in
                Expect.equal a (LWWMap.remove "A" "" a)
        , test ".remove removes an item from the map" <|
            \_ ->
                LWWMap.init
                    |> LWWMap.insert 0 "A" "foo" "bar"
                    |> LWWMap.remove "A" "foo"
                    |> LWWMap.get "foo"
                    |> Expect.equal Nothing
        , test "we can insert an item even after we removed it" <|
            \_ ->
                LWWMap.init
                    |> LWWMap.insert 0 "A" "foo" "bar"
                    |> LWWMap.remove "A" "foo"
                    |> LWWMap.insert 0 "A" "foo" "bar"
                    |> LWWMap.get "foo"
                    |> Expect.equal (Just "bar")
        , test ".get returns nothing on an empty map" <|
            \_ ->
                LWWMap.init |> LWWMap.get "" |> Expect.equal Nothing
        , fuzz2 int int "merging concurrent updates merges the updated values" <|
            \i j ->
                let
                    map =
                        LWWMap.init |> LWWMap.insert 0 "A" "foo" 0

                    a =
                        LWWMap.insert i "A" "foo" i map

                    b =
                        LWWMap.insert j "B" "foo" j map

                    merged =
                        LWWMap.merge a b
                in
                LWWMap.get "foo" merged
                    |> Expect.equal (Just (max i j))
        , test ".toDict returns the data as a dict" <|
            \_ ->
                LWWMap.init
                    |> LWWMap.insert 0 "A" "foo" "foo"
                    |> LWWMap.insert 0 "A" "bar" "bar"
                    |> LWWMap.toDict
                    |> Expect.equal (Dict.fromList [ ( "foo", "foo" ), ( "bar", "bar" ) ])
        ]
