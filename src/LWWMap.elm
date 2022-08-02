module LWWMap exposing (LWWMap, applyDiff, delta, get, init, insert, makeDiff, member, merge, remove, toDict)

import AWORMap exposing (DiffStatus, ReplicaId)
import Dict exposing (Dict)
import LWWReg


type LWWMap comparableTs comparableKey a
    = LWWMap (AWORMap.AWORMap comparableKey (LWWReg.LWWReg comparableTs a))


init : LWWMap comparableTs comparableKey a
init =
    LWWMap AWORMap.init


insert :
    comparableTs
    -> ReplicaId
    -> comparableKey
    -> v
    -> LWWMap comparableTs comparableKey v
    -> LWWMap comparableTs comparableKey v
insert ts rid k v (LWWMap map) =
    AWORMap.insert rid k (LWWReg.init ts v) map |> LWWMap


remove :
    ReplicaId
    -> comparableKey
    -> LWWMap comparableTs comparableKey v
    -> LWWMap comparableTs comparableKey v
remove rid k (LWWMap map) =
    AWORMap.remove rid k map |> LWWMap


merge :
    LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
merge (LWWMap a) (LWWMap b) =
    AWORMap.merge LWWReg.merge a b |> LWWMap


delta :
    LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
delta (LWWMap a) (LWWMap b) =
    AWORMap.delta (\x _ -> x) a b |> LWWMap


toDict : LWWMap comparableTs comparableKey a -> Dict comparableKey a
toDict (LWWMap map) =
    AWORMap.toDict LWWReg.value map


makeDiff :
    (a -> a -> valueDiff)
    -> LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
    -> Dict comparableKey (DiffStatus (LWWReg.LWWReg comparableTs a) valueDiff)
makeDiff diff (LWWMap a) (LWWMap b) =
    AWORMap.makeDiff LWWReg.value diff a b


applyDiff :
    (comparableTs -> comparableTs)
    -> comparableTs
    -> ReplicaId
    -> (valueDiff -> a -> a)
    -> Dict comparableKey (DiffStatus (LWWReg.LWWReg comparableTs a) valueDiff)
    -> LWWMap comparableTs comparableKey a
    -> LWWMap comparableTs comparableKey a
applyDiff nextTick ts rid applyValueDiff diff (LWWMap map) =
    AWORMap.applyDiff rid
        (\r d -> LWWReg.update nextTick ts (applyValueDiff d))
        diff
        map
        |> LWWMap


member : comparableKey -> LWWMap comparableTs comparableKey a -> Bool
member k (LWWMap map) =
    AWORMap.member k map


get : comparableKey -> LWWMap comparableTs comparableKey a -> Maybe a
get k (LWWMap map) =
    AWORMap.get k map |> Maybe.map LWWReg.value
