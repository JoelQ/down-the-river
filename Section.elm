module Section
    exposing
        ( Section
        , Block
        , Savepoint(..)
        , ObstacleArrangement(..)
        , empty
        , first
        , width
        , blocks
        , obstaclePositions
        , wolfPosition
        )

import Measurement exposing (Feet(..), addFeet, timesUnits, rawFeet)
import Coordinate


type alias Section =
    { save : Savepoint
    , b1 : ObstacleArrangement
    , b2 : ObstacleArrangement
    , b3 : ObstacleArrangement
    , extras : List ObstacleArrangement
    , last : ObstacleArrangement
    }


type ObstacleArrangement
    = TwoTop
    | TwoBottom
    | OneTop
    | OneBottom
    | OneMiddle
    | TopAndBottom
    | ClearWater


type Savepoint
    = TopWolf
    | BottomWolf
    | NoWolf


width : Section -> Feet
width section =
    let
        extrasWidth =
            blockWidth |> timesUnits (List.length section.extras)

        constantWidth =
            blockWidth |> timesUnits 5
    in
        addFeet extrasWidth constantWidth


empty : Section
empty =
    { save = NoWolf
    , b1 = ClearWater
    , b2 = ClearWater
    , b3 = ClearWater
    , extras = []
    , last = ClearWater
    }


first : Section
first =
    { save = TopWolf
    , b1 = OneTop
    , b2 = OneBottom
    , b3 = ClearWater
    , extras = []
    , last = TopAndBottom
    }



-- BLOCK


type Block
    = ObstacleBlock ObstacleArrangement
    | SaveBlock Savepoint


blocks : Section -> List Block
blocks { save, b1, b2, b3, extras, last } =
    [ SaveBlock save, ObstacleBlock b1, ObstacleBlock b2, ObstacleBlock b3 ]
        ++ (List.map ObstacleBlock extras)
        ++ [ ObstacleBlock last ]


blockWidth : Feet
blockWidth =
    Feet 20


wolfPosition : Int -> Block -> Maybe Coordinate.World
wolfPosition offset block =
    let
        xPosition =
            blockWidth
                |> timesUnits offset
                |> rawFeet
                |> toFloat

        top =
            60

        bottom =
            20
    in
        case block of
            SaveBlock TopWolf ->
                Just <| Coordinate.world xPosition top

            SaveBlock BottomWolf ->
                Just <| Coordinate.world xPosition bottom

            _ ->
                Nothing


obstaclePositions : Int -> Block -> Maybe (List Coordinate.World)
obstaclePositions offset block =
    case block of
        ObstacleBlock arr ->
            Just (positionsFromArrangement offset arr)

        _ ->
            Nothing


positionsFromArrangement : Int -> ObstacleArrangement -> List Coordinate.World
positionsFromArrangement offset arr =
    let
        xPosition =
            blockWidth
                |> timesUnits offset
                |> rawFeet
                |> toFloat
    in
        List.map (Coordinate.world xPosition) (arrangementYs arr)


arrangementYs : ObstacleArrangement -> List Float
arrangementYs arr =
    let
        top =
            50

        middle =
            42

        bottom =
            35
    in
        case arr of
            TwoTop ->
                [ top, middle ]

            TwoBottom ->
                [ middle, bottom ]

            OneTop ->
                [ top ]

            OneBottom ->
                [ bottom ]

            OneMiddle ->
                [ middle ]

            TopAndBottom ->
                [ top, bottom ]

            ClearWater ->
                []
