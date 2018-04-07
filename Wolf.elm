module Wolf exposing (toRenderable)

import Measurement exposing (Feet(..))
import Coordinate
import Renderable exposing (Renderable)


toRenderable : Coordinate.World -> Renderable
toRenderable position =
    { width = width
    , height = height
    , position = position
    , imagePath = imagePath
    }



-- CONSTANTS


width : Feet
width =
    Feet 6


height : Feet
height =
    Feet 8


imagePath : String
imagePath =
    "images/wolf.png"
