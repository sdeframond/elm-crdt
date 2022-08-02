module LWWReg exposing (LWWReg, init, makeDiff, merge, update, value)


type alias ReplicaId =
    String


type alias Clock =
    ( Int, ReplicaId )


type LWWReg a
    = LWWReg Clock a


init : Clock -> a -> LWWReg a
init cl v =
    LWWReg cl v


merge : LWWReg a -> LWWReg a -> LWWReg a
merge (LWWReg clA va) (LWWReg clB vb) =
    case compare clA clB of
        GT ->
            LWWReg clA va

        LT ->
            LWWReg clB vb

        EQ ->
            -- both A and B would do
            LWWReg clA va


value : LWWReg a -> a
value (LWWReg _ v) =
    v


makeDiff : (a -> a -> diff) -> LWWReg a -> LWWReg a -> diff
makeDiff diff (LWWReg _ a) (LWWReg _ b) =
    diff a b


update : Clock -> (a -> b) -> LWWReg a -> LWWReg b
update newCl f (LWWReg cl v) =
    LWWReg (max newCl cl) (f v)
