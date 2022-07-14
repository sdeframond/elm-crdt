module AWORSet exposing (AWORSet, delta, init, insert, member, merge, remove)

import AWORMap


type alias ReplicaId =
    String


type AWORSet comparable
    = AWORSet (AWORMap.AWORMap comparable ())


init : AWORSet comparable
init =
    AWORSet AWORMap.init


insert : ReplicaId -> comparable -> AWORSet comparable -> AWORSet comparable
insert rid v (AWORSet m) =
    AWORSet (AWORMap.insert rid v () m)


remove : ReplicaId -> comparable -> AWORSet comparable -> AWORSet comparable
remove rid v (AWORSet m) =
    AWORSet (AWORMap.remove rid v m)


merge : AWORSet comparable -> AWORSet comparable -> AWORSet comparable
merge (AWORSet a) (AWORSet b) =
    AWORSet (AWORMap.merge (\_ _ -> ()) a b)


member : comparable -> AWORSet comparable -> Bool
member v (AWORSet m) =
    AWORMap.member v m


delta : AWORSet comparable -> AWORSet comparable -> AWORSet comparable
delta (AWORSet a) (AWORSet b) =
    AWORSet (AWORMap.delta (\_ _ -> ()) a b)
