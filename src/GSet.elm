module GSet exposing (GSet, delta, init, insert, member, merge, toSet)

import Set


type GSet comparable
    = GSet (Set.Set comparable)


init : GSet comparable
init =
    GSet Set.empty


insert : comparable -> GSet comparable -> GSet comparable
insert v (GSet set) =
    GSet (Set.insert v set)


merge : GSet comparable -> GSet comparable -> GSet comparable
merge (GSet a) (GSet b) =
    Set.union a b |> GSet


toSet : GSet comparable -> Set.Set comparable
toSet (GSet set) =
    set


member : comparable -> GSet comparable -> Bool
member v (GSet set) =
    Set.member v set


delta : GSet comparable -> GSet comparable -> GSet comparable
delta (GSet a) (GSet b) =
    Set.diff a b |> GSet
