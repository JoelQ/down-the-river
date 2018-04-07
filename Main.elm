module Main exposing (main)

import AnimationFrame
import Collage
import Collision
import Color
import Coordinate exposing (Viewport)
import Element exposing (Element)
import GameText
import Html exposing (Html)
import Keyboard
import Measurement exposing (Feet(..), Pixels(..))
import Mouse
import Time exposing (Time)
import Random
import River exposing (River)
import River.Random
import Section exposing (ObstacleArrangement(..), Section)


type Model
    = Playing GameState
    | Lost GameState LossReason
    | Won GameState


type LossReason
    = HitObstacle
    | StrandedOnShore


type YDirection
    = Up
    | Down
    | Drifting


type alias GameState =
    { river : River
    , twinPosition : Coordinate.World
    , yDirection : YDirection
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
    | Move YDirection
    | NewSection Section
    | StartGame


initialModel : Model
initialModel =
    Playing initialGameState


initialGameState : GameState
initialGameState =
    { twinPosition = Coordinate.world 41 41
    , river = River.initial
    , yDirection = Drifting
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            tick diff model

        Move direction ->
            move direction model

        NewSection section ->
            appendNewSection section model

        StartGame ->
            startGame model


startGame : Model -> ( Model, Cmd a )
startGame model =
    case model of
        Lost _ _ ->
            Playing initialGameState
                |> withNoCmd

        Won _ ->
            Playing initialGameState
                |> withNoCmd

        Playing _ ->
            model
                |> withNoCmd


appendNewSection : Section -> Model -> ( Model, Cmd a )
appendNewSection section model =
    case model of
        Won _ ->
            model |> withNoCmd

        Lost _ _ ->
            model |> withNoCmd

        Playing state ->
            { state | river = River.appendSection section state.river }
                |> Playing
                |> withNoCmd


move : YDirection -> Model -> ( Model, Cmd Msg )
move direction model =
    case model of
        Playing state ->
            { state | yDirection = direction }
                |> Playing
                |> withNoCmd

        _ ->
            model
                |> withNoCmd


tick : Time -> Model -> ( Model, Cmd Msg )
tick diff model =
    case model of
        Playing state ->
            state
                |> moveTwinsDownstream diff
                |> checkLoseCondition
                |> andThen checkArrivalOnBank
                |> generateNewSectionsIfNecessary

        Lost _ _ ->
            model |> withNoCmd

        Won _ ->
            model |> withNoCmd


generateNewSectionsIfNecessary : Model -> ( Model, Cmd Msg )
generateNewSectionsIfNecessary model =
    case model of
        Won _ ->
            model |> withNoCmd

        Lost _ _ ->
            model |> withNoCmd

        Playing state ->
            let
                (Feet distance) =
                    Coordinate.distanceBetween state.twinPosition
                        (River.eastEdge state.river)

                needToGenerateDistance =
                    50

                cmd =
                    Random.generate NewSection (River.Random.section ClearWater)
            in
                if distance < needToGenerateDistance then
                    ( model, cmd )
                else
                    model |> withNoCmd


withNoCmd : a -> ( a, Cmd msg )
withNoCmd value =
    ( value, Cmd.none )


riverFeetPerSecond : Feet
riverFeetPerSecond =
    Feet 25


twinsFeetPerSecond : Feet
twinsFeetPerSecond =
    Feet 20


distanceTravelledInInterval : Feet -> Time -> Float
distanceTravelledInInterval (Feet distanceSecond) diff =
    (Time.inSeconds diff) * (toFloat distanceSecond)


moveTwinsDownstream : Time -> GameState -> GameState
moveTwinsDownstream diff state =
    let
        downstreamDistance =
            distanceTravelledInInterval riverFeetPerSecond diff

        accrossDistance =
            case state.yDirection of
                Up ->
                    distanceTravelledInInterval twinsFeetPerSecond diff

                Down ->
                    distanceTravelledInInterval twinsFeetPerSecond diff
                        |> negate

                Drifting ->
                    0

        newPosition =
            state.twinPosition
                |> Coordinate.moveBy downstreamDistance accrossDistance
    in
        { state | twinPosition = newPosition }


andThen : (GameState -> Model) -> Model -> Model
andThen func game =
    case game of
        Playing state ->
            func state

        _ ->
            game


checkLoseCondition : GameState -> Model
checkLoseCondition ({ twinPosition, river } as state) =
    let
        obstacles =
            List.map logToBoundingBox (River.logs river)

        subject =
            twinsToBoundingBox twinPosition
    in
        if subject |> Collision.hasCollidedWithAny obstacles then
            Lost state HitObstacle
        else
            Playing state


maxDistanceToWolf : Feet
maxDistanceToWolf =
    Feet 10


checkArrivalOnBank : GameState -> Model
checkArrivalOnBank state =
    if hasArrivedOnBank state then
        if closeEnoughToWolf state then
            Won state
        else
            Lost state StrandedOnShore
    else
        Playing state


closeEnoughToWolf : GameState -> Bool
closeEnoughToWolf state =
    List.any (closeEnough state.twinPosition) (River.wolves state.river)


closeEnough : Coordinate.World -> Coordinate.World -> Bool
closeEnough twins wolf =
    let
        (Feet distance) =
            Coordinate.distanceBetween twins wolf

        (Feet max) =
            maxDistanceToWolf
    in
        distance < max


hasArrivedOnBank : GameState -> Bool
hasArrivedOnBank { twinPosition, river } =
    let
        northBank =
            Coordinate.worldY river.position

        southBank =
            (Coordinate.worldY river.position)
                + (toFloat <| Measurement.rawFeet River.accross)

        twins =
            twinsToBoundingBox twinPosition
    in
        (twins |> Collision.hasCrossedHorizontalLine northBank)
            || (twins |> Collision.hasCrossedHorizontalLine southBank)


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


wolfWidth : Feet
wolfWidth =
    Feet 5


wolfHeight : Feet
wolfHeight =
    Feet 7


wolf : Element
wolf =
    let
        width =
            Measurement.feetToRawPixels wolfWidth

        height =
            Measurement.feetToRawPixels wolfHeight
    in
        Collage.rect (toFloat width) (toFloat height)
            |> Collage.filled Color.lightCharcoal
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


faded : Element -> Element
faded element =
    element
        |> Element.opacity 0.5


view : Model -> Html a
view model =
    case model of
        Playing state ->
            viewGameState state
                |> Element.toHtml

        Lost state StrandedOnShore ->
            [ viewGameState state |> faded
            , GameText.strandedOnShoreScreen
            ]
                |> Element.layers
                |> Element.toHtml

        Lost state HitObstacle ->
            [ viewGameState state |> faded
            , GameText.hitObstacleScreen
            ]
                |> Element.layers
                |> Element.toHtml

        Won state ->
            [ viewGameState state |> faded
            , GameText.winScreen
            ]
                |> Element.layers
                |> Element.toHtml


viewNature : Viewport -> Element
viewNature viewport =
    let
        (Pixels width) =
            viewport.width

        (Pixels height) =
            viewport.height
    in
        Collage.collage width
            height
            [ background viewport
            ]


viewGameState : GameState -> Element
viewGameState state =
    let
        viewport =
            viewportFor state.twinPosition

        nature =
            viewNature viewport

        boys =
            positionAt viewport (Coordinate.toScreen viewport state.twinPosition) twins

        wolves =
            List.map
                (\wolfPos ->
                    positionAt viewport (Coordinate.toScreen viewport wolfPos) wolf
                )
                (River.wolves state.river)
                |> Element.layers

        obstacles =
            List.map
                (\logPos ->
                    positionAt viewport (Coordinate.toScreen viewport logPos) log
                )
                (River.logs state.river)
                |> Element.layers

        renderedRiver =
            positionAt viewport
                (Coordinate.toScreen viewport state.river.position)
                (River.render state.river)
    in
        Element.layers [ nature, renderedRiver, boys, obstacles, wolves ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Lost _ _ ->
            Mouse.clicks (always StartGame)

        Won _ ->
            Mouse.clicks (always StartGame)

        Playing _ ->
            Sub.batch
                [ AnimationFrame.diffs Tick
                , Keyboard.downs keypressMessage
                , Keyboard.ups (\_ -> Move Drifting)
                ]


keypressMessage : Keyboard.KeyCode -> Msg
keypressMessage code =
    case code of
        38 ->
            Move Up

        40 ->
            Move Down

        _ ->
            Move Drifting


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
