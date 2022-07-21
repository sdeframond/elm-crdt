module DiffUndoListTest exposing (suite)

import DiffUndoList as UndoList
import Expect
import Fuzz exposing (Fuzzer, int)
import Test exposing (..)


fuzzer : Fuzzer (UndoList.UndoList Int Int)
fuzzer =
    Fuzz.map UndoList.init int


makeDiff : Int -> Int -> Int
makeDiff next initial =
    next - initial


apply : Int -> Int -> Int
apply d value =
    value + d


suite : Test
suite =
    describe "DiffUndoList"
        [ fuzz2 fuzzer int ".undo returns the value returned by its apply function when there is some history" <|
            \ul i ->
                UndoList.recordUpdate (\_ _ -> 42) (always 42) ul
                    |> UndoList.undo (\_ _ -> 42) (\_ _ -> i)
                    |> UndoList.value
                    |> Expect.equal i
        , fuzz int ".undo changes nothing when there is no history" <|
            \i ->
                UndoList.init i
                    |> UndoList.undo (\_ _ -> 42) (\_ _ -> 42)
                    |> Expect.equal (UndoList.init i)
        , fuzz2 fuzzer int ".redo returns the value returned by its apply function when there is some history" <|
            \ul i ->
                UndoList.recordUpdate (\_ _ -> 42) (always 42) ul
                    |> UndoList.undo (\_ _ -> 42) (\_ _ -> 42)
                    |> UndoList.redo (\_ _ -> 42) (\_ _ -> i)
                    |> UndoList.value
                    |> Expect.equal i
        , fuzz fuzzer "recordUpdate |> undo |> redo |> undo returns to the value set by 'recordUpdate'" <|
            \ul ->
                UndoList.recordUpdate makeDiff (always 42) ul
                    |> UndoList.undo makeDiff apply
                    |> UndoList.redo makeDiff apply
                    |> UndoList.value
                    |> Expect.equal 42
        , fuzz int ".redo changes nothing when there is no history" <|
            \i ->
                UndoList.init i
                    |> UndoList.redo (\_ _ -> 42) (\_ _ -> 42)
                    |> Expect.equal (UndoList.init i)
        , fuzz2 fuzzer int ".update updates the value" <|
            \ul i ->
                UndoList.update (always i) ul
                    |> UndoList.value
                    |> Expect.equal i
        , fuzz fuzzer ".update is not undoable" <|
            \ul ->
                UndoList.update ((+) 1) ul
                    |> UndoList.undo makeDiff apply
                    |> UndoList.value
                    |> Expect.equal (UndoList.value ul + 1)
        , fuzz2 fuzzer int ".recordUpdate updates the value" <|
            \ul i ->
                UndoList.recordUpdate makeDiff (always i) ul
                    |> UndoList.value
                    |> Expect.equal i
        ]
