module AWORMap exposing
    ( AWORMap
    , Operation
    , apply
    , delta
    , get
    , init
    , insert
    , makeInsert
    , makeRemove
    , makeUpdate
    , member
    , merge
    , remove
    , toDict
    , unapply
    , update
    )

import Dict
import VClock


type alias Dot v =
    ( VClock.VClock, Status v )


type AWORMap comparable v
    = AWORMap (Dict.Dict comparable (Dot v))


type alias ReplicaId =
    String


type Status v
    = Added v
    | Removed


type Operation k v valueOp
    = Insert k v (Maybe v)
    | Update k valueOp
    | Remove k (Maybe v)


init : AWORMap comparable v
init =
    AWORMap Dict.empty


insert : ReplicaId -> comparable -> v -> AWORMap comparable v -> AWORMap comparable v
insert rid k v (AWORMap d) =
    case Dict.get k d of
        Nothing ->
            AWORMap (Dict.insert k ( VClock.increment rid VClock.init, Added v ) d)

        Just ( vcl, Added current ) ->
            if v == current then
                AWORMap d

            else
                AWORMap (Dict.insert k ( VClock.increment rid vcl, Added v ) d)

        Just ( vcl, Removed ) ->
            AWORMap (Dict.insert k ( VClock.increment rid vcl, Added v ) d)


remove : ReplicaId -> comparable -> AWORMap comparable v -> AWORMap comparable v
remove rid k (AWORMap d) =
    case Dict.get k d of
        Nothing ->
            AWORMap d

        Just ( vcl, Added _ ) ->
            AWORMap (Dict.insert k ( VClock.increment rid vcl, Removed ) d)

        Just ( _, Removed ) ->
            AWORMap d


merge : (v -> v -> v) -> AWORMap comparable v -> AWORMap comparable v -> AWORMap comparable v
merge mergeValues (AWORMap da) (AWORMap db) =
    let
        mergeStatus stA stB =
            case ( stA, stB ) of
                ( Added a, Added b ) ->
                    Added (mergeValues a b)

                ( Added a, Removed ) ->
                    Added a

                ( Removed, Added b ) ->
                    Added b

                ( Removed, Removed ) ->
                    Removed

        mergeDots ( vclA, stA ) ( vclB, stB ) =
            ( VClock.merge vclA vclB, mergeStatus stA stB )

        mergeBoth k a b d =
            Dict.insert k (mergeDots a b) d
    in
    Dict.merge Dict.insert mergeBoth Dict.insert da db Dict.empty
        |> AWORMap


delta : (v -> v -> v) -> AWORMap comparable v -> AWORMap comparable v -> AWORMap comparable v
delta deltaValues (AWORMap da) (AWORMap db) =
    let
        skip _ _ =
            identity

        statusDelta stA stB =
            case ( stA, stB ) of
                ( Added a, Added b ) ->
                    Added (deltaValues a b)

                ( Added a, Removed ) ->
                    Added a

                ( Removed, _ ) ->
                    -- We don't care about b
                    Removed

        insertDelta k ( vclA, stA ) ( vclB, stB ) d =
            let
                clockDelta =
                    VClock.delta vclA vclB
            in
            if clockDelta == VClock.init then
                d

            else
                Dict.insert k ( clockDelta, statusDelta stA stB ) d
    in
    AWORMap <| Dict.merge Dict.insert insertDelta skip da db Dict.empty


member : comparable -> AWORMap comparable v -> Bool
member k (AWORMap d) =
    case Dict.get k d of
        Nothing ->
            False

        Just ( _, Added _ ) ->
            True

        Just ( _, Removed ) ->
            False


get : comparable -> AWORMap comparable v -> Maybe v
get k (AWORMap d) =
    Dict.get k d
        |> Maybe.andThen
            (\( _, st ) ->
                case st of
                    Added v ->
                        Just v

                    Removed ->
                        Nothing
            )


update : (ReplicaId -> op -> v -> v) -> ReplicaId -> comparable -> op -> AWORMap comparable v -> AWORMap comparable v
update applyValueOp rid k op map =
    get k map
        |> Maybe.map (applyValueOp rid op)
        |> Maybe.map (\v -> insert rid k v map)
        |> Maybe.withDefault map


toDict : (v -> a) -> AWORMap comparable v -> Dict.Dict comparable a
toDict value (AWORMap d) =
    let
        filterDot k dot res =
            case dot of
                ( _, Removed ) ->
                    res

                ( _, Added v ) ->
                    Dict.insert k (value v) res
    in
    Dict.foldl filterDot Dict.empty d


apply :
    ReplicaId
    -> (ReplicaId -> valueOp -> v -> v)
    -> Operation comparable v valueOp
    -> AWORMap comparable v
    -> AWORMap comparable v
apply rid applyValueOp op map =
    case op of
        Insert k v _ ->
            insert rid k v map

        Remove k _ ->
            remove rid k map

        Update k valueOp ->
            update applyValueOp rid k valueOp map


unapply :
    ReplicaId
    -> (ReplicaId -> valueOp -> v -> v)
    -> Operation comparable v valueOp
    -> AWORMap comparable v
    -> AWORMap comparable v
unapply rid unapplyValueOp op map =
    case op of
        Insert k _ mv ->
            case mv of
                Nothing ->
                    remove rid k map

                Just replaced ->
                    insert rid k replaced map

        Remove k mv ->
            case mv of
                Nothing ->
                    map

                Just removed ->
                    insert rid k removed map

        Update k valueOp ->
            update unapplyValueOp rid k valueOp map


makeInsert : comparable -> v -> AWORMap comparable v -> Operation comparable v valueOp
makeInsert k v map =
    Insert k v (get k map)


makeRemove : comparable -> AWORMap comparable v -> Operation comparable v valueOp
makeRemove k map =
    Remove k (get k map)


makeUpdate : b -> c -> d -> Operation b v c
makeUpdate k op _ =
    Update k op
