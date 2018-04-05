module River.Random exposing (section)

import Section exposing (Section, ObstacleArrangement(..), Savepoint(..))
import Random exposing (Generator)
import Random.Extra
import Random.Lookback


section : ObstacleArrangement -> Generator Section
section arr =
    Random.Lookback.constant arr Section
        |> Random.Lookback.andMap savepoint
        |> Random.Lookback.andMapBoth obstacleArrangement
        |> Random.Lookback.andMapBoth obstacleArrangement
        |> Random.Lookback.andMapBoth obstacleArrangement
        |> Random.Lookback.andMapList obstacleList
        |> Random.Lookback.andMapBoth obstacleArrangement
        |> Random.Lookback.toNormalGenerator


obstacleList : ObstacleArrangement -> Generator (List ObstacleArrangement)
obstacleList arr =
    nextObstacleInList arr (Random.Extra.constant [])


nextObstacleInList :
    ObstacleArrangement
    -> Generator (List ObstacleArrangement)
    -> Generator (List ObstacleArrangement)
nextObstacleInList arr gen =
    Random.Extra.maybe Random.bool (obstacleArrangement arr)
        |> Random.andThen
            (\maybe ->
                case maybe of
                    Just this ->
                        nextObstacleInList this (Random.map (\list -> this :: list) gen)

                    Nothing ->
                        gen
            )


savepoint : Generator Savepoint
savepoint =
    Random.Extra.sample [ TopWolf, BottomWolf, NoWolf ]
        |> Random.map (Maybe.withDefault NoWolf)


obstacleArrangement : ObstacleArrangement -> Generator ObstacleArrangement
obstacleArrangement arr =
    arr
        |> allowedNextBlocks
        |> Random.Extra.sample
        |> Random.map (Maybe.withDefault ClearWater)


allowedNextBlocks : ObstacleArrangement -> List ObstacleArrangement
allowedNextBlocks arr =
    case arr of
        TwoTop ->
            [ TwoTop, OneTop, OneMiddle, ClearWater ]

        TwoBottom ->
            [ TwoBottom, OneBottom, OneMiddle, ClearWater ]

        OneTop ->
            [ TwoTop, OneTop, OneBottom, OneMiddle, TopAndBottom, ClearWater ]

        OneBottom ->
            [ TwoBottom, OneTop, OneBottom, OneMiddle, TopAndBottom, ClearWater ]

        OneMiddle ->
            [ OneMiddle, ClearWater ]

        TopAndBottom ->
            [ OneTop, OneBottom, TopAndBottom, ClearWater ]

        ClearWater ->
            [ TwoTop, TwoBottom, OneTop, OneBottom, OneMiddle, TopAndBottom, ClearWater ]
