module OpUndoList exposing
    ( UndoList
    , do
    , init
    , redo
    , undo
    , value
    )


type UndoList a op
    = UL
        { undo : List op
        , redo : List op
        , value : a
        }


init : a -> UndoList a op
init a =
    UL
        { undo = []
        , redo = []
        , value = a
        }


value : UndoList a op -> a
value (UL data) =
    data.value


do : (op -> a -> b) -> op -> UndoList a op -> UndoList b op
do apply op (UL data) =
    UL
        { value = apply op data.value
        , undo = op :: data.undo
        , redo = data.redo
        }


undo : (op -> a -> b) -> UndoList a op -> Maybe (UndoList b op)
undo unapply (UL data) =
    case data.undo of
        [] ->
            Nothing

        prevOp :: rest ->
            Just
                (UL
                    { value = unapply prevOp data.value
                    , undo = rest
                    , redo = prevOp :: data.redo
                    }
                )


redo : (op -> a -> b) -> UndoList a op -> Maybe (UndoList b op)
redo apply (UL data) =
    case data.redo of
        [] ->
            Nothing

        nextOp :: rest ->
            Just
                (UL
                    { value = apply nextOp data.value
                    , undo = nextOp :: data.undo
                    , redo = rest
                    }
                )
