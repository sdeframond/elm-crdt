module OpUndoListTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import OpUndoList as UndoList
import Test exposing (..)


type alias Op =
    Int


apply : Op -> Int -> Int
apply op i =
    i + op


unapply : Op -> Int -> Int
unapply op i =
    i - op


operation : Fuzzer Op
operation =
    Fuzz.int


undoList : Fuzzer (UndoList.UndoList Int Op)
undoList =
    let
        actions =
            [ operation |> Fuzz.map (UndoList.do apply)
            , (\ul -> UndoList.undo unapply ul |> Maybe.withDefault ul) |> Fuzz.constant
            , (\ul -> UndoList.redo apply ul |> Maybe.withDefault ul) |> Fuzz.constant
            ]

        applyOps ops =
            List.foldr (\f x -> f x) (UndoList.init 0) ops
    in
    actions |> Fuzz.oneOf |> Fuzz.list |> Fuzz.map applyOps


suite : Test
suite =
    describe "OpUndoList"
        [ test ".value returns the value" <|
            \_ ->
                UndoList.init 1 |> UndoList.value |> Expect.equal 1
        , fuzz2 undoList operation ".do applies the operation" <|
            \ul op ->
                UndoList.do apply op ul
                    |> UndoList.value
                    |> Expect.equal (op + UndoList.value ul)
        , fuzz2 undoList operation ".undo restores the previous state" <|
            \ul op ->
                UndoList.do apply op ul
                    |> UndoList.undo unapply
                    |> Maybe.map UndoList.value
                    |> Expect.equal (Just <| UndoList.value ul)
        , fuzz2 undoList operation ".redo restores the next state" <|
            \ul op ->
                let
                    next =
                        UndoList.do apply op ul
                in
                UndoList.undo unapply next
                    |> Maybe.andThen (UndoList.redo apply)
                    |> Maybe.map UndoList.value
                    |> Expect.equal (Just <| UndoList.value next)
        ]
