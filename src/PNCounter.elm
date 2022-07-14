module PNCounter exposing
    ( PNCounter
    , decrement
    , delta
    , increment
    , init
    , merge
    , value
    )

import GCounter exposing (GCounter)


type alias ReplicaId =
    String


type PNCounter
    = PNCounter GCounter GCounter


init : PNCounter
init =
    PNCounter GCounter.zero GCounter.zero


value : PNCounter -> Int
value (PNCounter incs decs) =
    GCounter.value incs - GCounter.value decs


increment : ReplicaId -> PNCounter -> PNCounter
increment rid (PNCounter incs decs) =
    PNCounter
        (GCounter.increment rid incs)
        decs


decrement : ReplicaId -> PNCounter -> PNCounter
decrement rid (PNCounter incs decs) =
    PNCounter
        incs
        (GCounter.increment rid decs)


merge : PNCounter -> PNCounter -> PNCounter
merge (PNCounter incsA decsA) (PNCounter incsB decsB) =
    PNCounter
        (GCounter.merge incsA incsB)
        (GCounter.merge decsA decsB)


delta : PNCounter -> PNCounter -> PNCounter
delta (PNCounter incsA decsA) (PNCounter incsB decsB) =
    PNCounter (GCounter.delta incsA incsB) (GCounter.delta decsA decsB)
