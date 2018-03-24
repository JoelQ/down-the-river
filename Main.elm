module Main exposing (main)

import AnimationFrame
import Collage
import Color
import Element exposing (Element)
import Euclid.Vector as Vector
import Html exposing (Html)
import Time exposing (Time)


type Feet
    = Feet Int


type WorldCoordinate
    = WorldCoordinate (Vector.V2 Float)


type ScreenCoordinate
    = ScreenCoordinate (Vector.V2 Int)


type LocalCoordinate
    = LocalCoordinate (Vector.V2 Float)


moveRelative : LocalCoordinate -> WorldCoordinate -> WorldCoordinate
moveRelative (LocalCoordinate moveBy) (WorldCoordinate initialPosition) =
    Vector.add moveBy initialPosition
        |> WorldCoordinate


toScreen : WorldCoordinate -> ScreenCoordinate
toScreen (WorldCoordinate vector) =
    vector
        |> Vector.scale pixelsPerFoot
        |> Vector.map round
        |> ScreenCoordinate


type alias Model =
    { riverWidth : Feet
    , twinPosition : WorldCoordinate
    , logs : List WorldCoordinate
    }


type Msg
    = Tick Time


logs : List WorldCoordinate
logs =
    [ WorldCoordinate (Vector.vec 60 39)
    , WorldCoordinate (Vector.vec 75 50)
    , WorldCoordinate (Vector.vec 100 35)
    ]


initialModel : Model
initialModel =
    { riverWidth = Feet 30
    , twinPosition = WorldCoordinate (Vector.vec 41 41)
    , logs = logs
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            ( moveTwinsDownstream diff model, Cmd.none )


riverFeetPerSecond : Feet
riverFeetPerSecond =
    Feet 10


moveTwinsDownstream : Time -> Model -> Model
moveTwinsDownstream diff model =
    let
        (Feet distanceSecond) =
            riverFeetPerSecond

        distanceTravelled =
            (Time.inSeconds diff) * (toFloat distanceSecond)

        newPosition =
            model.twinPosition
                |> moveRelative (LocalCoordinate (Vector.vec distanceTravelled 0))
    in
        { model | twinPosition = newPosition }


type Pixels
    = Pixels Int


pixelsPerFoot : number
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


twins : Element
twins =
    Collage.rect 35 25
        |> Collage.filled Color.brown
        |> List.singleton
        |> Collage.collage 35 25


log : Element
log =
    Collage.rect 45 25
        |> Collage.filled Color.darkBrown
        |> List.singleton
        |> Collage.collage 45 25


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
            positionAt (toScreen model.twinPosition) twins

        obstacles =
            List.map (\logPos -> positionAt (toScreen logPos) log) model.logs
                |> Element.layers
    in
        Element.layers [ nature, boys, obstacles ]
            |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
