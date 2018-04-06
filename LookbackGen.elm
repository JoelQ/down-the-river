module LookbackGen exposing (LookbackGen)

import Random exposing (Generator)


type Lookback a b
    = Lookback a b


type LookbackGen a b
    = LookbackGen (Generator (Lookback a b))


lookbackAndThen : Lookback a b -> (b -> Lookback b c) -> Lookback b c
lookbackAndThen (Lookback _ value) func =
    func value


andThen : LookbackGen a b -> (b -> LookbackGen b c) -> LookbackGen b c
andThen (LookbackGen gen) func =
    gen |> andThen (\lb -> lb |> lookbackAndThen func)
