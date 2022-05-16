module AWORSet exposing (AWORSet, init, insert, member, merge, remove)

import Dict
import VClock


type AWORSet comparable
    = AWORSet (Dict.Dict comparable ( VClock.VClock, Status ))


type alias ReplicaId =
    String


type Status
    = Added
    | Removed


statusMax : Status -> Status -> Status
statusMax a b =
    case ( a, b ) of
        ( Added, _ ) ->
            Added

        ( _, Added ) ->
            Added

        _ ->
            Removed


init : AWORSet comparable
init =
    AWORSet Dict.empty


insert : ReplicaId -> comparable -> AWORSet comparable -> AWORSet comparable
insert rid v (AWORSet d) =
    case Dict.get v d of
        Nothing ->
            AWORSet (Dict.insert v ( VClock.zero, Added ) d)

        Just ( vcl, Added ) ->
            AWORSet (Dict.insert v ( vcl, Added ) d)

        Just ( vcl, Removed ) ->
            AWORSet (Dict.insert v ( VClock.increment rid vcl, Added ) d)


remove : ReplicaId -> comparable -> AWORSet comparable -> AWORSet comparable
remove rid v (AWORSet d) =
    case Dict.get v d of
        Nothing ->
            AWORSet d

        Just ( vcl, Added ) ->
            AWORSet (Dict.insert v ( VClock.increment rid vcl, Removed ) d)

        Just ( vcl, Removed ) ->
            AWORSet (Dict.insert v ( vcl, Removed ) d)


merge : AWORSet comparable -> AWORSet comparable -> AWORSet comparable
merge (AWORSet da) (AWORSet db) =
    let
        mergeStatus ( vclA, stA ) ( vclB, stB ) =
            case ( VClock.compare vclA vclB, stA, stB ) of
                ( VClock.EQ, _, _ ) ->
                    ( vclA, statusMax stA stB )

                ( VClock.GT, _, _ ) ->
                    ( vclA, stA )

                ( VClock.LT, _, _ ) ->
                    ( vclB, stB )

                ( VClock.CC, Removed, Removed ) ->
                    ( VClock.merge vclA vclB, Removed )

                ( VClock.CC, _, _ ) ->
                    ( VClock.merge vclA vclB, Added )

        merge_ v a b res =
            Dict.insert v (mergeStatus a b) res
    in
    Dict.merge Dict.insert merge_ Dict.insert da db Dict.empty
        |> AWORSet


member : comparable -> AWORSet comparable -> Bool
member v (AWORSet d) =
    case Dict.get v d of
        Nothing ->
            False

        Just ( _, Added ) ->
            True

        Just ( _, Removed ) ->
            False
