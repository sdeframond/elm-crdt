module GCounter exposing (GCounter, increment, merge, value, zero)

import Dict exposing (Dict)


type alias ReplicaId =
    String


type GCounter
    = GCounter (Dict ReplicaId Int)


zero : GCounter
zero =
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
