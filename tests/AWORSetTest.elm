module AWORSetTest exposing (suite)

import AWORSet
import CrdtHelper exposing (isASimpleCrdt)
import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string, tuple3)
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


replica : Fuzzer String
replica =
    [ "A", "B", "C", "D" ]
        |> List.map constant
        |> oneOf


operationsFuzzer : Fuzzer (List ( String, Operation, String ))
operationsFuzzer =
    let
        operation =
            [ Insert, Remove ]
                |> List.map constant
                |> oneOf
    in
    tuple3 ( replica, operation, string )
        |> list


fuzzer : Fuzzer (AWORSet.AWORSet String)
fuzzer =
    Fuzz.map applyOps operationsFuzzer


suite : Test
suite =
    describe "AWORSet"
        [ isASimpleCrdt { fuzzer = fuzzer, merge = AWORSet.merge }
        , testMergeConflict
        , testInsertAndRemove
        ]


testMergeConflict : Test
testMergeConflict =
    fuzz3 replica replica fuzzer "Add wins over a concurrent remove" <|
        \ridA ridB set ->
            AWORSet.merge
                (AWORSet.remove ridA "" set)
                (AWORSet.remove ridB "" set |> AWORSet.insert "B" "")
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
        , fuzz2 replica fuzzer ".insert is idempotent" <|
            \rid set ->
                let
                    a =
                        AWORSet.insert rid "" set
                in
                Expect.equal a (AWORSet.insert rid "" a)
        , fuzz2 replica fuzzer ".remove is idempotent" <|
            \rid set ->
                let
                    a =
                        AWORSet.remove rid "" set
                in
                Expect.equal a (AWORSet.remove rid "" a)
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
