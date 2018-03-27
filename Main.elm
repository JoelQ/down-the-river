module Main exposing (main)

import AnimationFrame
import Collage
import Collision
import Color
import Coordinate exposing (Viewport)
import Element exposing (Element)
import Html exposing (Html)
import Measurement exposing (Feet(..), Pixels(..))
import Time exposing (Time)


type Model
    = Playing GameState
    | Lost GameState


type alias GameState =
    { riverWidth : Feet
    , twinPosition : Coordinate.World
    , logs : List Coordinate.World
    }


initialViewport : Viewport
initialViewport =
    { position = Coordinate.world 0 0
    , width = viewportWidth
    , height = viewportHeight
    }


viewportFor : Coordinate.World -> Viewport
viewportFor twinPosition =
    let
        twinX =
            Coordinate.worldX twinPosition

        widthInFeet =
            Measurement.pixelsToFeet viewportWidth

        halfViewport =
            (toFloat <| Measurement.rawFeet widthInFeet) / 2

        edgeX =
            twinX - halfViewport
    in
        { position = Coordinate.world edgeX 0
        , width = viewportWidth
        , height = viewportHeight
        }


viewportHeight : Pixels
viewportHeight =
    Pixels 500


viewportWidth : Pixels
viewportWidth =
    Pixels 800


type Msg
    = Tick Time


logs : List Coordinate.World
logs =
    [ Coordinate.world 60 39
    , Coordinate.world 75 50
    , Coordinate.world 100 35
    ]


initialModel : Model
initialModel =
    Playing initialGameState


initialGameState : GameState
initialGameState =
    { riverWidth = Feet 30
    , twinPosition = Coordinate.world 41 41
    , logs = logs
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            case model of
                Playing state ->
                    state
                        |> moveTwinsDownstream diff
                        |> checkLoseCondition
                        |> withNoCmd

                Lost _ ->
                    model
                        |> withNoCmd


withNoCmd : a -> ( a, Cmd msg )
withNoCmd value =
    ( value, Cmd.none )


riverFeetPerSecond : Feet
riverFeetPerSecond =
    Feet 10


moveTwinsDownstream : Time -> GameState -> GameState
moveTwinsDownstream diff state =
    let
        (Feet distanceSecond) =
            riverFeetPerSecond

        distanceTravelled =
            (Time.inSeconds diff) * (toFloat distanceSecond)

        newPosition =
            state.twinPosition
                |> Coordinate.moveX distanceTravelled
    in
        { state | twinPosition = newPosition }


checkLoseCondition : GameState -> Model
checkLoseCondition ({ twinPosition, logs } as state) =
    let
        obstacles =
            (List.map logToBoundingBox logs)

        subject =
            twinsToBoundingBox twinPosition
    in
        if subject |> Collision.hasCollidedWithAny obstacles then
            Lost state
        else
            Playing state


logToBoundingBox : Coordinate.World -> Collision.BoundingBox
logToBoundingBox position =
    { position = position
    , width = logWidth
    , height = logHeight
    }


twinsToBoundingBox : Coordinate.World -> Collision.BoundingBox
twinsToBoundingBox position =
    { position = position
    , width = twinWidth
    , height = twinHeight
    }


background : Viewport -> Collage.Form
background viewport =
    let
        (Pixels width) =
            viewport.width

        (Pixels height) =
            viewport.height
    in
        Collage.rect (toFloat width) (toFloat height)
            |> Collage.filled Color.green


river : Viewport -> Feet -> Collage.Form
river viewport feet =
    let
        width =
            Measurement.feetToRawPixels feet

        (Pixels height) =
            viewport.width
    in
        Collage.rect (toFloat height) (toFloat width)
            |> Collage.filled Color.blue


twinHeight : Feet
twinHeight =
    Feet 5


twinWidth : Feet
twinWidth =
    Feet 6


twins : Element
twins =
    let
        width =
            Measurement.feetToRawPixels twinWidth

        height =
            Measurement.feetToRawPixels twinHeight
    in
        Collage.rect (toFloat width) (toFloat height)
            |> Collage.filled Color.brown
            |> List.singleton
            |> Collage.collage width height


logWidth : Feet
logWidth =
    Feet 8


logHeight : Feet
logHeight =
    Feet 5


log : Element
log =
    let
        width =
            Measurement.feetToRawPixels logWidth

        height =
            Measurement.feetToRawPixels logHeight
    in
        Collage.rect (toFloat width) (toFloat height)
            |> Collage.filled Color.darkBrown
            |> List.singleton
            |> Collage.collage width height


positionAt : Viewport -> Coordinate.Screen -> Element -> Element
positionAt viewport position element =
    let
        x =
            Element.absolute (Coordinate.screenX position)

        y =
            Element.absolute (Coordinate.screenY position)

        (Pixels width) =
            viewport.width

        (Pixels height) =
            viewport.height
    in
        Element.container width height (Element.bottomLeftAt x y) element


view : Model -> Html a
view model =
    case model of
        Playing state ->
            viewGameState state

        Lost state ->
            viewGameState state


viewNature : Viewport -> Feet -> Element
viewNature viewport riverWidth =
    let
        (Pixels width) =
            viewport.width

        (Pixels height) =
            viewport.height
    in
        Collage.collage width
            height
            [ background viewport
            , river viewport riverWidth
            ]


viewGameState : GameState -> Html a
viewGameState state =
    let
        viewport =
            viewportFor state.twinPosition

        nature =
            viewNature viewport state.riverWidth

        boys =
            positionAt viewport (Coordinate.toScreen viewport state.twinPosition) twins

        obstacles =
            List.map
                (\logPos ->
                    positionAt viewport (Coordinate.toScreen viewport logPos) log
                )
                state.logs
                |> Element.layers
    in
        Element.layers [ nature, boys, obstacles ]
            |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Lost _ ->
            Sub.none

        Playing _ ->
            AnimationFrame.diffs Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
