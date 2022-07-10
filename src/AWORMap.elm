module AWORMap exposing
    ( AWORMap
    , get
    , init
    , insert
    , member
    , merge
    , remove
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


init : AWORMap comparable v
init =
    AWORMap Dict.empty


insert : ReplicaId -> comparable -> v -> AWORMap comparable v -> AWORMap comparable v
insert rid k v (AWORMap d) =
    case Dict.get k d of
        Nothing ->
            AWORMap (Dict.insert k ( VClock.increment rid VClock.zero, Added v ) d)

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
            case VClock.compare vclA vclB of
                VClock.EQ ->
                    -- Both values should be equal, excepted when concurrent
                    -- operations were applied with the same replica id, which
                    -- is not a supported case anyway.
                    ( vclA, stA )

                VClock.CC ->
                    ( VClock.merge vclA vclB, mergeStatus stA stB )

                VClock.GT ->
                    ( vclA, stA )

                VClock.LT ->
                    ( vclB, stB )

        mergeBoth k a b d =
            Dict.insert k (mergeDots a b) d
    in
    Dict.merge Dict.insert mergeBoth Dict.insert da db Dict.empty
        |> AWORMap


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
