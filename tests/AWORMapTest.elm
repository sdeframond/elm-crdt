module AWORMapTest exposing (suite)

import AWORMap
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string)
import GCounter
import GCounterTest
import Helpers exposing (itIsACrdt, itIsDiffable)
import Test exposing (..)


type Operation
    = Insert String String GCounter.GCounter
    | Remove String String


applyOps : List Operation -> AWORMap.AWORMap String GCounter.GCounter
applyOps ops =
    let
        apply op map =
            case op of
                Insert rid k v ->
                    AWORMap.insert rid k v map

                Remove rid k ->
                    AWORMap.remove rid k map
    in
    List.foldl apply AWORMap.init ops


replica : List String -> Fuzzer String
replica rids =
    List.map constant rids
        |> oneOf


operationsFuzzer : List String -> Fuzzer (List Operation)
operationsFuzzer rids =
    let
        operation =
            [ Fuzz.map3 Insert (replica rids) string GCounterTest.fuzzer
            , Fuzz.map2 Remove (replica rids) string
            ]
                |> oneOf
    in
    list operation


fuzzer : List String -> Fuzzer (AWORMap.AWORMap String GCounter.GCounter)
fuzzer rids =
    Fuzz.map applyOps (operationsFuzzer rids)


suite : Test
suite =
    describe "AWORMap"
        [ itIsACrdt
            { fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            , merge = AWORMap.merge GCounter.merge
            }
        , itIsDiffable
            { init = AWORMap.init
            , merge = AWORMap.merge GCounter.merge
            , delta = AWORMap.delta GCounter.delta
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B", "C" ]
            }
        , fuzz2 (fuzzer [ "A", "B" ])
            GCounterTest.fuzzer
            "Add wins over a concurrent remove"
          <|
            \map value ->
                AWORMap.merge GCounter.merge
                    (AWORMap.remove "A" "" map)
                    (AWORMap.remove "B" "" map |> AWORMap.insert "B" "" value)
                    |> AWORMap.member ""
                    |> Expect.true "Expect the empty string to be in the map"
        , test ".insert puts an item into the map" <|
            \_ ->
                AWORMap.init
                    |> AWORMap.insert "A" "foo" "bar"
                    |> AWORMap.get "foo"
                    |> Expect.equal (Just "bar")
        , fuzz2 (fuzzer [ "A", "B" ]) GCounterTest.fuzzer ".insert is idempotent" <|
            \map value ->
                let
                    a =
                        AWORMap.insert "A" "" value map
                in
                Expect.equal a (AWORMap.insert "A" "" value a)
        , fuzz (fuzzer [ "A", "B" ]) ".remove is idempotent" <|
            \map ->
                let
                    a =
                        AWORMap.remove "A" "" map
                in
                Expect.equal a (AWORMap.remove "A" "" a)
        , test ".remove removes an item from the map" <|
            \_ ->
                AWORMap.init
                    |> AWORMap.insert "A" "foo" "bar"
                    |> AWORMap.remove "A" "foo"
                    |> AWORMap.get "foo"
                    |> Expect.equal Nothing
        , test "we can insert an item even after we removed it" <|
            \_ ->
                AWORMap.init
                    |> AWORMap.insert "A" "foo" "bar"
                    |> AWORMap.remove "A" "foo"
                    |> AWORMap.insert "A" "foo" "bar"
                    |> AWORMap.get "foo"
                    |> Expect.equal (Just "bar")
        , test ".get returns nothing on an empty map" <|
            \_ ->
                AWORMap.init |> AWORMap.get "" |> Expect.equal Nothing
        , test "merging concurrent updates merges the updated values" <|
            \_ ->
                let
                    map =
                        AWORMap.init |> AWORMap.insert "A" "foo" GCounter.init

                    a =
                        AWORMap.insert "A" "foo" (GCounter.init |> GCounter.increment "A") map

                    b =
                        AWORMap.insert "B" "foo" (GCounter.init |> GCounter.increment "B") map

                    merged =
                        AWORMap.merge GCounter.merge a b

                    expectedValue =
                        GCounter.init
                            |> GCounter.increment "B"
                            |> GCounter.increment "A"
                in
                AWORMap.get "foo" merged
                    |> Expect.equal (Just expectedValue)
        ]
