module AWORMapTest exposing (suite)

import AWORMap
import Dict
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string)
import Helpers exposing (itIsACrdt, itIsADelta, itIsUndoable)
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


actionMaker : List String -> Fuzzer (AWORMap -> ( String, Operation ))
actionMaker rids =
    let
        makeInsert rid k v map =
            ( rid, AWORMap.makeInsert k v map )

        makeRemove rid k map =
            ( rid, AWORMap.makeRemove k map )

        makeUpdate rid k v map =
            ( rid, AWORMap.makeUpdate k v map )
    in
    oneOf
        [ Fuzz.map3 makeInsert (replica rids) string PNCounterTest.fuzzer
        , Fuzz.map2 makeRemove (replica rids) string
        , Fuzz.map3 makeUpdate (replica rids) string PNCounterTest.operation
        ]


fuzzer : List String -> Fuzzer AWORMap
fuzzer rids =
    let
        applyAction makeAction map =
            let
                ( rid, op ) =
                    makeAction map
            in
            AWORMap.apply rid PNCounter.apply op map

        applyActions opMakers =
            List.foldl applyAction AWORMap.init opMakers
    in
    Fuzz.map applyActions (list (actionMaker rids))


suite : Test
suite =
    describe "AWORMap"
        [ itIsACrdt
            { fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            , merge = AWORMap.merge PNCounter.merge
            }
        , itIsADelta
            { init = AWORMap.init
            , merge = AWORMap.merge PNCounter.merge
            , delta = AWORMap.delta PNCounter.delta
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B", "C" ]
            }
        , itIsUndoable
            { apply = \( rid, op ) -> AWORMap.apply rid PNCounter.apply op
            , unapply = \( rid, op ) -> AWORMap.unapply rid PNCounter.unapply op
            , value = AWORMap.toDict PNCounter.value
            , fuzzData = fuzzer [ "A", "B", "C" ]
            , fuzzOpMaker = actionMaker [ "A" ]
            }
        , Helpers.itIsValueDiffable
            { fuzzer = fuzzer [ "A", "B" ]
            , makeDiff = AWORMap.makeDiff PNCounter.value (-)
            , apply = AWORMap.applyDiff "A" PNCounter.addInt
            , value = AWORMap.toDict PNCounter.value
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
