module AWORMapTest exposing (suite)

import AWORMap
import Dict
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string)
import Helpers exposing (itIsACrdt, itIsDiffable, itIsUndoable)
import PNCounter
import PNCounterTest
import Test exposing (..)


type alias Operation =
    AWORMap.Operation String PNCounter.PNCounter PNCounter.Operation


type alias AWORMap =
    AWORMap.AWORMap String PNCounter.PNCounter


replica : List String -> Fuzzer String
replica rids =
    List.map constant rids
        |> oneOf


operation : List String -> Fuzzer (AWORMap -> Operation)
operation rids =
    oneOf
        [ Fuzz.map3 AWORMap.makeInsert (replica rids) string PNCounterTest.fuzzer
        , Fuzz.map2 AWORMap.makeRemove (replica rids) string
        , Fuzz.map3 AWORMap.makeUpdate (replica rids) string PNCounterTest.operation
        ]


fuzzer : List String -> Fuzzer AWORMap
fuzzer rids =
    let
        applyOps : List (AWORMap -> Operation) -> AWORMap
        applyOps ops =
            List.foldl (\makeOp map -> AWORMap.apply PNCounter.apply (makeOp map) map) AWORMap.init ops
    in
    Fuzz.map applyOps (operation rids |> list)


suite : Test
suite =
    describe "AWORMap"
        [ itIsACrdt
            { fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            , merge = AWORMap.merge PNCounter.merge
            }
        , itIsDiffable
            { init = AWORMap.init
            , merge = AWORMap.merge PNCounter.merge
            , delta = AWORMap.delta PNCounter.delta
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B", "C" ]
            }
        , itIsUndoable
            { apply = AWORMap.apply PNCounter.apply
            , unapply = AWORMap.unapply PNCounter.unapply
            , value = AWORMap.toDict PNCounter.value
            , fuzzData = fuzzer [ "A", "B", "C" ]
            , fuzzOpMaker = operation [ "A" ]
            }
        , fuzz2 (fuzzer [ "A", "B" ])
            PNCounterTest.fuzzer
            "Add wins over a concurrent remove"
          <|
            \map value ->
                AWORMap.merge PNCounter.merge
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
        , fuzz2 (fuzzer [ "A", "B" ]) PNCounterTest.fuzzer ".insert is idempotent" <|
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
                        AWORMap.init |> AWORMap.insert "A" "foo" PNCounter.init

                    a =
                        AWORMap.insert "A" "foo" (PNCounter.init |> PNCounter.increment "A") map

                    b =
                        AWORMap.insert "B" "foo" (PNCounter.init |> PNCounter.increment "B") map

                    merged =
                        AWORMap.merge PNCounter.merge a b

                    expectedValue =
                        PNCounter.init
                            |> PNCounter.increment "B"
                            |> PNCounter.increment "A"
                in
                AWORMap.get "foo" merged
                    |> Expect.equal (Just expectedValue)
        , fuzz2 (fuzzer [ "A", "B" ]) PNCounterTest.operation ".update applies an operation to the value" <|
            \map valueOp ->
                AWORMap.update PNCounter.apply "A" "" valueOp map
                    |> AWORMap.get ""
                    |> Expect.equal (AWORMap.get "" map |> Maybe.map (PNCounter.apply "A" valueOp))
        , test ".toDict returns the data as a dict" <|
            \_ ->
                AWORMap.init
                    |> AWORMap.insert "A" "foo" "foo"
                    |> AWORMap.insert "A" "bar" "bar"
                    |> AWORMap.toDict identity
                    |> Expect.equal (Dict.fromList [ ( "foo", "foo" ), ( "bar", "bar" ) ])
        ]
