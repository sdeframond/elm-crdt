module Helpers exposing
    ( itIsACrdt
    , itIsADelta
    , itIsAnAnonymousCrdt
    , itIsAnonymouslyDiffable
    , itIsUndoable
    , itIsValueDiffable
    )

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


itIsAnAnonymousCrdt : { fuzzer : Fuzzer a, merge : a -> a -> a } -> Test
itIsAnAnonymousCrdt { fuzzer, merge } =
    itIsACrdt
        { fuzzerA = fuzzer
        , fuzzerB = fuzzer
        , fuzzerC = fuzzer
        , merge = merge
        }


itIsACrdt :
    { fuzzerA : Fuzzer a
    , fuzzerB : Fuzzer a
    , fuzzerC : Fuzzer a
    , merge : a -> a -> a
    }
    -> Test
itIsACrdt { fuzzerA, fuzzerB, fuzzerC, merge } =
    describe "it is a CRDT"
        [ fuzz fuzzerA "it is idempotent" <|
            \crdt -> merge crdt crdt |> Expect.equal crdt
        , fuzz2 fuzzerA fuzzerB "it is commutative" <|
            \a b -> merge a b |> Expect.equal (merge b a)
        , fuzz3 fuzzerA fuzzerB fuzzerC "it is associative" <|
            \a b c ->
                merge a (merge b c)
                    |> Expect.equal (merge a b |> merge c)
        ]


itIsAnonymouslyDiffable :
    { init : a
    , fuzzer : Fuzz.Fuzzer a
    , delta : a -> a -> a
    , merge : a -> a -> a
    }
    -> Test
itIsAnonymouslyDiffable { init, fuzzer, delta, merge } =
    itIsADelta
        { init = init
        , fuzzerA = fuzzer
        , fuzzerB = fuzzer
        , delta = delta
        , merge = merge
        }


itIsADelta :
    { init : a
    , delta : a -> a -> a
    , merge : a -> a -> a
    , fuzzerA : Fuzz.Fuzzer a
    , fuzzerB : Fuzz.Fuzzer a
    }
    -> Test
itIsADelta { init, fuzzerA, fuzzerB, delta, merge } =
    describe "it is a delta"
        [ fuzz2 fuzzerA fuzzerB "a + d(b, a) == a + b" <|
            \a b ->
                Expect.equal (merge a (delta b a)) (merge a b)
        , fuzz fuzzerA "d(a, init) == a" <|
            \a ->
                Expect.equal (delta a init) a
        , fuzz2 fuzzerA fuzzerB "d(a, a + b) == init" <|
            \a b ->
                Expect.equal (delta a (merge a b)) init
        ]


itIsValueDiffable :
    { fuzzer : Fuzz.Fuzzer a
    , makeDiff : a -> a -> diff
    , apply : diff -> a -> a
    , value : a -> v
    }
    -> Test
itIsValueDiffable { fuzzer, makeDiff, apply, value } =
    describe "it is diffable"
        [ fuzz2 fuzzer fuzzer "v(apply(d(a, b), b)) == v(a)" <|
            \a b ->
                value (apply (makeDiff a b) b)
                    |> Expect.equal (value a)
        ]


itIsUndoable :
    { apply : op -> a -> a
    , unapply : op -> a -> a
    , value : a -> b
    , fuzzData : Fuzz.Fuzzer a
    , fuzzOpMaker : Fuzz.Fuzzer (a -> op)
    }
    -> Test
itIsUndoable { apply, unapply, value, fuzzData, fuzzOpMaker } =
    describe "it is undoable"
        [ fuzz2 fuzzData fuzzOpMaker "unapplying an operation restores the inital value" <|
            \data makeOp ->
                let
                    op =
                        makeOp data
                in
                apply op data
                    |> unapply op
                    |> value
                    |> Expect.equal (value data)
        ]
