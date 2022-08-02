module LWWReg exposing (LWWReg, init, makeDiff, merge, update, value)


type LWWReg comparable a
    = LWWReg comparable a


init : comparable -> a -> LWWReg comparable a
init ts v =
    LWWReg ts v


merge : LWWReg comparable a -> LWWReg comparable a -> LWWReg comparable a
merge (LWWReg tsA va) (LWWReg tsB vb) =
    case compare tsA tsB of
        GT ->
            LWWReg tsA va

        LT ->
            LWWReg tsB vb

        EQ ->
            -- Timestamps equality must imply values equality in order for
            -- LWWReg to be a CRDT.
            LWWReg tsA va


value : LWWReg comparable a -> a
value (LWWReg _ v) =
    v


makeDiff : (a -> a -> diff) -> LWWReg comparable a -> LWWReg comparable a -> diff
makeDiff diff (LWWReg _ a) (LWWReg _ b) =
    diff a b


update : (comparable -> comparable) -> comparable -> (a -> b) -> LWWReg comparable a -> LWWReg comparable b
update nextTick ts f (LWWReg oldTs v) =
    LWWReg (max ts (nextTick oldTs)) (f v)
