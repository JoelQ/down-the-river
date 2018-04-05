module Random.Lookback
    exposing
        ( andMap
        , andMapBoth
        , andMapList
        , constant
        , toNormalGenerator
        )

import Random exposing (Generator)
import Random.Extra


type alias Lookback prev value =
    ( prev, value )


type alias LookbackGen prev value =
    Generator (Lookback prev value)


constant : a -> b -> LookbackGen a b
constant prev this =
    Random.Extra.constant ( prev, this )


andMapBoth : (a -> Generator a) -> LookbackGen a (a -> b) -> LookbackGen a b
andMapBoth gen funcGen =
    funcGen
        |> Random.andThen
            (\( prev, func ) ->
                Random.map (\val -> ( val, func val )) <| gen prev
            )


andMapList :
    (a -> Generator (List a))
    -> LookbackGen a (List a -> b)
    -> LookbackGen a b
andMapList gen funcGen =
    funcGen
        |> Random.andThen
            (\( prev, f ) ->
                Random.map
                    (\list ->
                        ( List.head list |> Maybe.withDefault prev, f list )
                    )
                <|
                    gen prev
            )


andMap : Generator a -> LookbackGen prev (a -> b) -> LookbackGen prev b
andMap gen funcGen =
    funcGen
        |> Random.andThen (\( prev, func ) -> Random.map (\val -> ( prev, func val )) gen)


toNormalGenerator : LookbackGen a b -> Generator b
toNormalGenerator =
    Random.map Tuple.second
