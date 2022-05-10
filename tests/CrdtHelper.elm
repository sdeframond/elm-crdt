module CrdtHelper exposing (itIsACrdt)

import Expect
import Fuzz exposing (Fuzzer, tuple, tuple3)
import Test exposing (..)


itIsACrdt : { fuzzer : Fuzzer a, merge : a -> a -> a } -> Test
itIsACrdt { fuzzer, merge } =
    describe "it is a CRDT"
        [ fuzz fuzzer "it is idempotent" <|
            \gset -> merge gset gset |> Expect.equal gset
        , fuzz (tuple ( fuzzer, fuzzer )) "it is commutative" <|
            \( a, b ) -> merge a b |> Expect.equal (merge b a)
        , fuzz (tuple3 ( fuzzer, fuzzer, fuzzer )) "it is associative" <|
            \( a, b, c ) ->
                merge a (merge b c)
                    |> Expect.equal (merge a b |> merge c)
        ]
