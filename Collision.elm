module Collision
    exposing
        ( BoundingBox
        , hasCrossedHorizontalLine
        , hasCollidedWithAny
        )

import Coordinate
import Measurement exposing (Feet(..))


type alias BoundingBox =
    { position : Coordinate.World
    , width : Feet
    , height : Feet
    }


hasCrossedHorizontalLine : Float -> BoundingBox -> Bool
hasCrossedHorizontalLine y box =
    let
        height =
            toFloat (Measurement.rawFeet box.height)

        bottom =
            Coordinate.worldY box.position

        top =
            bottom + height
    in
        (bottom < y && y < top)


hasCollidedWithAny : List BoundingBox -> BoundingBox -> Bool
hasCollidedWithAny objects subject =
    List.any (haveCollided subject) objects


haveCollided : BoundingBox -> BoundingBox -> Bool
haveCollided box1 box2 =
    let
        r1Width =
            Measurement.rawFeet box1.width

        r1Height =
            Measurement.rawFeet box1.height

        r2Width =
            Measurement.rawFeet box1.width

        r2Height =
            Measurement.rawFeet box1.height

        r1X =
            round (Coordinate.worldX box1.position)

        r1Y =
            round (Coordinate.worldY box1.position)

        r2X =
            round (Coordinate.worldX box2.position)

        r2Y =
            round (Coordinate.worldY box2.position)
    in
        (r1X < (r2X + r2Width))
            && ((r1X + r1Width) > r2X)
            && (r1Y < (r2Y + r2Height))
            && ((r1Height + r1Y) > r2Y)
