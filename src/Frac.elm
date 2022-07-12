module Frac exposing
    ( Frac
    , between
    , bottom
    , compare
    , top
    )

import BigRational as BR exposing (BigRational)


type Frac
    = Frac BigRational


bottom : Frac
bottom =
    Frac (BR.fromInt 0)


top : Frac
top =
    Frac (BR.fromInt 1)


between : Frac -> Frac -> Frac
between (Frac x) (Frac y) =
    Frac (BR.div (BR.add x y) (BR.fromInt 2))


compare : Frac -> Frac -> Order
compare (Frac x) (Frac y) =
    BR.compare x y
