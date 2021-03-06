module GCounter exposing (GCounter, delta, increment, init, merge, toDict, value)

import Dict exposing (Dict)


type alias ReplicaId =
    String


type GCounter
    = GCounter (Dict ReplicaId Int)


init : GCounter
init =
    GCounter Dict.empty


increment : ReplicaId -> GCounter -> GCounter
increment rid (GCounter d) =
    let
        update =
            Maybe.withDefault 0 >> (+) 1 >> Just
    in
    Dict.update rid update d |> GCounter


merge : GCounter -> GCounter -> GCounter
merge (GCounter da) (GCounter db) =
    Dict.merge
        (\k v d -> Dict.insert k v d)
        (\k va vb d -> Dict.insert k (max va vb) d)
        (\k v d -> Dict.insert k v d)
        da
        db
        Dict.empty
        |> GCounter


value : GCounter -> Int
value (GCounter d) =
    Dict.foldl (\_ v sum -> sum + v) 0 d


toDict : GCounter -> Dict ReplicaId Int
toDict (GCounter d) =
    d


delta : GCounter -> GCounter -> GCounter
delta (GCounter da) (GCounter db) =
    let
        skip _ _ =
            identity

        insertIfGreater k a b d =
            if a > b then
                Dict.insert k a d

            else
                d
    in
    GCounter (Dict.merge Dict.insert insertIfGreater skip da db Dict.empty)
