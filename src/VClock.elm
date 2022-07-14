module VClock exposing
    ( Order(..)
    , VClock
    , compare
    , delta
    , increment
    , merge
    , zero
    )

import Dict
import GCounter


type Order
    = GT -- Greater than
    | LT -- Lower than
    | EQ -- Equals
    | CC -- Can't compare


type VClock
    = VClock GCounter.GCounter


type alias ReplicaId =
    String


zero : VClock
zero =
    VClock GCounter.zero


increment : ReplicaId -> VClock -> VClock
increment rid (VClock c) =
    VClock (GCounter.increment rid c)


compare : VClock -> VClock -> Order
compare (VClock a_) (VClock b_) =
    let
        da =
            GCounter.toDict a_

        db =
            GCounter.toDict b_

        f _ a b prev =
            case ( prev, Basics.compare a b ) of
                ( EQ, Basics.EQ ) ->
                    EQ

                ( EQ, Basics.LT ) ->
                    LT

                ( EQ, Basics.GT ) ->
                    GT

                ( GT, Basics.LT ) ->
                    CC

                ( GT, _ ) ->
                    GT

                ( LT, Basics.GT ) ->
                    CC

                ( LT, _ ) ->
                    LT

                ( CC, _ ) ->
                    CC
    in
    Dict.merge (\k a -> f k a 0) f (\k b -> f k 0 b) da db EQ


merge : VClock -> VClock -> VClock
merge (VClock a) (VClock b) =
    VClock (GCounter.merge a b)


delta : VClock -> VClock -> VClock
delta (VClock a) (VClock b) =
    VClock (GCounter.delta a b)
