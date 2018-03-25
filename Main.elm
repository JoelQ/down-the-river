module Main exposing (main)

import AnimationFrame
import Collage
import Collision
import Color
import Coordinate
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


background : Collage.Form
background =
    Collage.rect 800 500
        |> Collage.filled Color.green


river : Feet -> Collage.Form
river feet =
    let
        width =
            Measurement.feetToRawPixels feet
    in
        Collage.rect 800 (toFloat width)
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


positionAt : Coordinate.Screen -> Element -> Element
positionAt position element =
    let
        x =
            Element.absolute (Coordinate.screenX position)

        y =
            Element.absolute (Coordinate.screenY position)
    in
        Element.container 800 500 (Element.bottomLeftAt x y) element


view : Model -> Html a
view model =
    case model of
        Playing state ->
            viewGameState state

        Lost state ->
            viewGameState state


viewGameState : GameState -> Html a
viewGameState state =
    let
        nature =
            Collage.collage 800 500 [ background, river state.riverWidth ]

        boys =
            positionAt (Coordinate.toScreen state.twinPosition) twins

        obstacles =
            List.map
                (\logPos ->
                    positionAt (Coordinate.toScreen logPos) log
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
