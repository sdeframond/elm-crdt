module AWORSetTest exposing (suite)

import AWORSet
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string, tuple3)
import Helpers exposing (itIsACrdt, itIsADelta)
import Test exposing (..)


type Operation
    = Insert
    | Remove


applyOps : List ( String, Operation, String ) -> AWORSet.AWORSet String
applyOps ops =
    let
        apply ( rid, op, arg ) set =
            case op of
                Insert ->
                    AWORSet.insert rid arg set

                Remove ->
                    AWORSet.remove rid arg set
    in
    List.foldl apply AWORSet.init ops


replica : List String -> Fuzzer String
replica rids =
    List.map constant rids |> oneOf


operationsFuzzer : List String -> Fuzzer (List ( String, Operation, String ))
operationsFuzzer rids =
    let
        operation =
            [ Insert, Remove ]
                |> List.map constant
                |> oneOf
    in
    tuple3 ( replica rids, operation, string )
        |> list


fuzzer : List String -> Fuzzer (AWORSet.AWORSet String)
fuzzer rids =
    Fuzz.map applyOps (operationsFuzzer rids)


suite : Test
suite =
    describe "AWORSet"
        [ itIsACrdt
            { fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            , fuzzerC = fuzzer [ "C" ]
            , merge = AWORSet.merge
            }
        , itIsADelta
            { init = AWORSet.init
            , merge = AWORSet.merge
            , delta = AWORSet.delta
            , fuzzerA = fuzzer [ "A" ]
            , fuzzerB = fuzzer [ "B" ]
            }
        , testMergeConflict
        , testInsertAndRemove
        ]


testMergeConflict : Test
testMergeConflict =
    fuzz (fuzzer [ "A", "B" ]) "Add wins over a concurrent remove" <|
        \set ->
            AWORSet.merge
                (AWORSet.remove "A" "" set)
                (AWORSet.remove "B" "" set |> AWORSet.insert "B" "")
                |> AWORSet.member ""
                |> Expect.true "Expect the empty string to be in the set"


testInsertAndRemove : Test
testInsertAndRemove =
    describe "insert and remove"
        [ test ".insert puts an item into the set" <|
            \_ ->
                AWORSet.init
                    |> AWORSet.insert "A" "foo"
                    |> AWORSet.member "foo"
                    |> Expect.true "Expect `foo` to be in the set"
        , fuzz (fuzzer [ "A", "B" ]) ".insert is idempotent" <|
            \set ->
                let
                    a =
                        AWORSet.insert "A" "" set
                in
                Expect.equal a (AWORSet.insert "A" "" a)
        , fuzz (fuzzer [ "A", "B" ]) ".remove is idempotent" <|
            \set ->
                let
                    a =
                        AWORSet.remove "A" "" set
                in
                Expect.equal a (AWORSet.remove "A" "" a)
        , test ".remove removes an item from the set" <|
            \_ ->
                AWORSet.init
                    |> AWORSet.insert "A" "foo"
                    |> AWORSet.remove "A" "foo"
                    |> AWORSet.member "foo"
                    |> Expect.false "Expect `foo` not to be in the set"
        , test "we can insert a item even after we removed it" <|
            \_ ->
                AWORSet.init
                    |> AWORSet.insert "A" "foo"
                    |> AWORSet.remove "A" "foo"
                    |> AWORSet.insert "A" "foo"
                    |> AWORSet.member "foo"
                    |> Expect.true "Expect `foo` to be in the set"
        ]
