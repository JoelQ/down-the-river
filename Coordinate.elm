module Coordinate
    exposing
        ( World
        , Screen
        , Object
        , Viewport
        , world
        , worldX
        , worldY
        , screenX
        , screenY
        , moveBy
        , toScreen
        )

import Euclid.Vector as Vector
import Measurement exposing (Pixels)


-- CONSTRUCTORS


type World
    = World (Vector.V2 Float)


type Screen
    = Screen (Vector.V2 Int)


type Object
    = Object (Vector.V2 Float)


type alias Viewport =
    { position : World
    , width : Pixels
    , height : Pixels
    }


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


moveBy : Float -> Float -> World -> World
moveBy x y position =
    move (relative x y) position


move : Object -> World -> World
move (Object offset) (World initialPosition) =
    Vector.add offset initialPosition
        |> World



-- CONVERSIONS


relativeTo : World -> World -> Object
relativeTo (World target) (World position) =
    Vector.subtract position target
        |> Object


toScreen : Viewport -> World -> Screen
toScreen viewport position =
    position
        |> relativeTo viewport.position
        |> objectVector
        |> Vector.scale Measurement.pixelsPerFoot
        |> Vector.map round
        |> Screen



-- UNWRAP


objectVector : Object -> Vector.V2 Float
objectVector (Object position) =
    position
