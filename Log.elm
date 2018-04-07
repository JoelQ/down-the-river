module Log exposing (toRenderable, toBoundingBox)

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
    , height = height
    }



-- CONSTANTS


width : Feet
width =
    Feet 10


height : Feet
height =
    Feet 4


imagePath : String
imagePath =
    "images/log.png"
