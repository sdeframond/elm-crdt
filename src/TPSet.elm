module TPSet exposing
    ( TPSet
    , delta
    , init
    , insert
    , member
    , merge
    , remove
    )

import GSet exposing (GSet)


type TPSet comparable
    = TPSet (GSet comparable) (GSet comparable)


init : TPSet comparable
init =
    TPSet GSet.empty GSet.empty


insert : comparable -> TPSet comparable -> TPSet comparable
insert v (TPSet added deleted) =
    if GSet.member v deleted then
        TPSet added deleted

    else
        TPSet (GSet.insert v added) deleted


remove : comparable -> TPSet comparable -> TPSet comparable
remove v (TPSet added deleted) =
    TPSet added (GSet.insert v deleted)


merge : TPSet comparable -> TPSet comparable -> TPSet comparable
merge (TPSet addedA deletedA) (TPSet addedB deletedB) =
    TPSet (GSet.merge addedA addedB) (GSet.merge deletedA deletedB)


member : comparable -> TPSet comparable -> Bool
member v (TPSet added deleted) =
    not (GSet.member v deleted) && GSet.member v added


delta : TPSet comparable -> TPSet comparable -> TPSet comparable
delta (TPSet addedA deletedA) (TPSet addedB deletedB) =
    TPSet (GSet.delta addedA addedB) (GSet.delta deletedA deletedB)
