module DiffUndoList exposing (UndoList, init, recordUpdate, redo, undo, update, value)


type UndoList a diff
    = UL { value : a, undo : List diff, redo : List diff }


init : a -> UndoList a diff
init a =
    UL
        { value = a
        , undo = []
        , redo = []
        }


undo : (a -> a -> diff) -> (diff -> a -> a) -> UndoList a diff -> UndoList a diff
undo makeDiff apply (UL data) =
    case data.undo of
        [] ->
            UL data

        toPrevious :: before ->
            let
                previous =
                    apply toPrevious data.value
            in
            UL { data | value = previous, undo = before, redo = makeDiff previous data.value :: data.redo }


redo : (a -> a -> diff) -> (diff -> a -> a) -> UndoList a diff -> UndoList a diff
redo makeDiff apply (UL data) =
    case data.redo of
        [] ->
            UL data

        toNext :: after ->
            let
                next =
                    apply toNext data.value
            in
            UL { data | value = next, redo = after, undo = makeDiff next data.value :: data.undo }


update : (a -> a) -> UndoList a diff -> UndoList a diff
update f (UL data) =
    UL { data | value = f data.value }


value : UndoList a diff -> a
value (UL data) =
    data.value


recordUpdate : (a -> a -> diff) -> (a -> a) -> UndoList a diff -> UndoList a diff
recordUpdate makeDiff f (UL data) =
    UL { data | value = f data.value, undo = makeDiff data.value data.value :: data.undo }
