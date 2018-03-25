module Coordinate
    exposing
        ( World
        , Screen
        , Object
        , world
        , worldX
        , worldY
        , screenX
        , screenY
        , moveX
        , toScreen
        )

import Euclid.Vector as Vector
import Measurement


-- CONSTRUCTORS


type World
    = World (Vector.V2 Float)


type Screen
    = Screen (Vector.V2 Int)


type Object
    = Object (Vector.V2 Float)


world : Float -> Float -> World
world x y =
    World (Vector.vec x y)


relative : Float -> Float -> Object
relative x y =
    Object (Vector.vec x y)



-- ACCESSORS


worldX : World -> Float
worldX (World position) =
    position.x


worldY : World -> Float
worldY (World position) =
    position.y


screenX : Screen -> Int
screenX (Screen position) =
    position.x


screenY : Screen -> Int
screenY (Screen position) =
    position.y



-- MOVEMENT


moveX : Float -> World -> World
moveX distance position =
    move (relative distance 0) position


move : Object -> World -> World
move (Object moveBy) (World initialPosition) =
    Vector.add moveBy initialPosition
        |> World



-- CONVERSIONS


toScreen : World -> Screen
toScreen (World vector) =
    vector
        |> Vector.scale Measurement.pixelsPerFoot
        |> Vector.map round
        |> Screen
