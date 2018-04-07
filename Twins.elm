module Twins exposing (toRenderable, toBoundingBox)

import Measurement exposing (Feet(..))
import Coordinate
import Collision
import Renderable exposing (Renderable)


toRenderable : Coordinate.World -> Renderable
toRenderable position =
    { width = width
    , height = height
    , position = position
    , imagePath = imagePath
    }


toBoundingBox : Coordinate.World -> Collision.BoundingBox
toBoundingBox position =
    { position = position
    , width = width
    , height = width
    }



-- CONSTANTS


width : Feet
width =
    Feet 6


height : Feet
height =
    Feet 5


imagePath : String
imagePath =
    "images/twins.png"
