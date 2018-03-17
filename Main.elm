module Main exposing (main)

import Html exposing (Html)
import Element exposing (Element)
import Collage
import Color
import Euclid.Vector as Vector


type Feet
    = Feet Int


type WorldCoordinate
    = WorldCoordinate (Vector.V2 Int)


type ScreenCoordinate
    = ScreenCoordinate (Vector.V2 Int)


toScreen : WorldCoordinate -> ScreenCoordinate
toScreen (WorldCoordinate vector) =
    vector
        |> Vector.scale pixelsPerFoot
        |> ScreenCoordinate


type alias Model =
    { riverWidth : Feet
    , twinPosition : WorldCoordinate
    }


type Msg
    = Noop


initialModel : Model
initialModel =
    { riverWidth = Feet 30
    , twinPosition = WorldCoordinate (Vector.vec 41 41)
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Pixels
    = Pixels Int


pixelsPerFoot : Int
pixelsPerFoot =
    6


feetToPixels : Feet -> Pixels
feetToPixels (Feet ft) =
    Pixels (ft * pixelsPerFoot)


background : Collage.Form
background =
    Collage.rect 800 500
        |> Collage.filled Color.green


river : Feet -> Collage.Form
river feet =
    let
        (Pixels width) =
            feetToPixels feet
    in
        Collage.rect 800 (toFloat width)
            |> Collage.filled Color.blue


twins : Collage.Form
twins =
    Collage.rect 35 25
        |> Collage.filled Color.brown


positionAt : ScreenCoordinate -> Element -> Element
positionAt (ScreenCoordinate vector) element =
    let
        x =
            Element.absolute vector.x

        y =
            Element.absolute vector.y
    in
        Element.container 800 500 (Element.bottomLeftAt x y) element


view : Model -> Html a
view model =
    let
        nature =
            Collage.collage 800 500 [ background, river model.riverWidth ]

        boys =
            positionAt (toScreen model.twinPosition) (Collage.collage 35 25 [ twins ])
    in
        Element.layers [ nature, boys ]
            |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
