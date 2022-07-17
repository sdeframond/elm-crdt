module PNCounter exposing
    ( Operation(..)
    , PNCounter
    , apply
    , decrement
    , delta
    , increment
    , init
    , merge
    , unapply
    , value
    )

import GCounter exposing (GCounter)


type alias ReplicaId =
    String


type PNCounter
    = PNCounter GCounter GCounter


type Operation
    = Inc
    | Dec


init : PNCounter
init =
    PNCounter GCounter.init GCounter.init


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


apply : ReplicaId -> Operation -> PNCounter -> PNCounter
apply rid op c =
    case op of
        Inc ->
            increment rid c

        Dec ->
            decrement rid c


unapply : ReplicaId -> Operation -> PNCounter -> PNCounter
unapply rid op c =
    case op of
        Inc ->
            decrement rid c

        Dec ->
            increment rid c
