module Renderable exposing (Renderable, render, positionAt)

import Measurement exposing (Feet)
import Coordinate exposing (Viewport)
import Element exposing (Element)


type alias Renderable =
    { width : Feet
    , height : Feet
    , position : Coordinate.World
    , imagePath : String
    }


render : Viewport -> Renderable -> Element
render viewport renderable =
    toElement renderable
        |> positionAt viewport (Coordinate.toScreen viewport renderable.position)


toElement : Renderable -> Element
toElement { width, height, imagePath } =
    Element.image
        (Measurement.feetToRawPixels width)
        (Measurement.feetToRawPixels height)
        imagePath


positionAt : Viewport -> Coordinate.Screen -> Element -> Element
positionAt viewport position element =
    let
        x =
            Element.absolute (Coordinate.screenX position)

        y =
            Element.absolute (Coordinate.screenY position)
    in
        Element.container
            (Measurement.rawPixels viewport.width)
            (Measurement.rawPixels viewport.height)
            (Element.bottomLeftAt x y)
            element
