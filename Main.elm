module Main exposing (main)

import Html exposing (Html)
import Element
import Collage
import Color


type Feet
    = Feet Int


type alias Model =
    { riverWidth : Feet
    }


type Msg
    = Noop


init : ( Model, Cmd Msg )
init =
    ( { riverWidth = Feet 30 }, Cmd.none )


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


view : Model -> Html a
view model =
    Collage.collage 800 500 [ background, river model.riverWidth ]
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
