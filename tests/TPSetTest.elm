module TPSetTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf, string, tuple)
import Helpers exposing (itIsAnAnonymousCrdt, itIsAnonymouslyDiffable)
import TPSet
import Test exposing (..)


type Operation
    = Insert
    | Remove


applyOps : List ( Operation, String ) -> TPSet.TPSet String
applyOps ops =
    let
        apply ( op, arg ) set =
            case op of
                Insert ->
                    TPSet.insert arg set

                Remove ->
                    TPSet.remove arg set
    in
    List.foldl apply TPSet.init ops


operationsFuzzer : Fuzzer (List ( Operation, String ))
operationsFuzzer =
    let
        operation =
            [ Insert, Remove ]
                |> List.map constant
                |> oneOf
    in
    tuple ( operation, string )
        |> list


fuzzer : Fuzzer (TPSet.TPSet String)
fuzzer =
    Fuzz.map applyOps operationsFuzzer


suite : Test
suite =
    describe "TPSet"
        [ itIsAnAnonymousCrdt { fuzzer = fuzzer, merge = TPSet.merge }
        , itIsAnonymouslyDiffable
            { init = TPSet.init
            , fuzzer = fuzzer
            , merge = TPSet.merge
            , delta = TPSet.delta
            }
        , test "inserted items are in the set" <|
            \_ ->
                TPSet.init
                    |> TPSet.insert "foo"
                    |> TPSet.member "foo"
                    |> Expect.true "Expect `foo` to be a member of the set"
        , test "deleted items are not in the set" <|
            \_ ->
                TPSet.init
                    |> TPSet.insert "foo"
                    |> TPSet.remove "foo"
                    |> TPSet.member "foo"
                    |> Expect.false "Expect `foo` not to be a member of the set"
        , test "deleted items cannot be inserted again" <|
            \_ ->
                TPSet.init
                    |> TPSet.insert "foo"
                    |> TPSet.remove "foo"
                    |> TPSet.insert "foo"
                    |> TPSet.member "foo"
                    |> Expect.false "Expect `foo` not to be a member of the set"
        ]
