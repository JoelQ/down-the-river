module Random.Lookback
    exposing
        ( myFun
        , myFunAnon
        , myFunList
        , constant
        , toNormalGenerator
        )

import Random exposing (Generator)
import Random.Extra
import Lookback exposing (Lookback)


type alias LookbackGen prev value =
    Generator (Lookback prev value)


constant : a -> b -> LookbackGen a b
constant prev this =
    Lookback.toLookback prev this
        |> Random.Extra.constant


toNormalGenerator : LookbackGen a b -> Generator b
toNormalGenerator =
    Random.map Lookback.value


myFun : (prev -> Generator new) -> LookbackGen prev (new -> b) -> LookbackGen new b
myFun toGen funcGen =
    funcGen |> andMap (smoosh toGen funcGen)


myFunList : (a -> Generator (List a)) -> LookbackGen a (List a -> b) -> LookbackGen a b
myFunList toListGen funcGen =
    let
        newGen =
            smoosh toListGen funcGen
                |> Random.map Lookback.prevFromListLast
    in
        funcGen |> andMapAnon newGen


myFunAnon : Generator new -> LookbackGen prev (new -> b) -> LookbackGen prev b
myFunAnon gen funcGen =
    funcGen |> andMapAnon (smoosh (always gen) funcGen)


andMap : LookbackGen prev a -> LookbackGen prev (a -> b) -> LookbackGen a b
andMap gen genFunc =
    Random.map2 (\lb lbFunc -> lbFunc |> Lookback.andMap lb) gen genFunc


andMapAnon : LookbackGen prev a -> LookbackGen prev (a -> b) -> LookbackGen prev b
andMapAnon gen genFunc =
    Random.map2 (\lb lbFunc -> lbFunc |> Lookback.andMapAnon lb) gen genFunc


smoosh : (prev -> Generator new) -> LookbackGen prev a -> LookbackGen prev new
smoosh toGen lbGen =
    Random.andThen (invert << Lookback.newValFromPrev toGen) lbGen


invert : Lookback a (Generator b) -> LookbackGen a b
invert lookback =
    Random.map (\val -> Lookback.mapAnon (always val) lookback)
        (Lookback.value lookback)
