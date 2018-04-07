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
import Renderable
import River.Random
import Section exposing (ObstacleArrangement(..), Section)
import Wolf
import Twins
import Log


type Model
    = Intro
    | Playing GameState
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


initialTwinPosition : Coordinate.World
initialTwinPosition =
    let
        (Feet width) =
            viewportWidth
                |> Measurement.pixelsToFeet
    in
        Coordinate.world (toFloat <| width // 2) 41


initialGameState : GameState
initialGameState =
    { twinPosition = initialTwinPosition
    , river = River.initial
    , yDirection = Drifting
    }


init : ( Model, Cmd Msg )
init =
    ( Intro, Cmd.none )


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


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    case model of
        Intro ->
            ( Playing initialGameState, generateNewSection )

        Lost _ _ ->
            ( Playing initialGameState, generateNewSection )

        Won _ ->
            ( Playing initialGameState, generateNewSection )

        Playing _ ->
            model |> withNoCmd


appendNewSection : Section -> Model -> ( Model, Cmd a )
appendNewSection section model =
    case model of
        Playing state ->
            { state | river = River.appendSection section state.river }
                |> Playing
                |> withNoCmd

        _ ->
            model |> withNoCmd


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

        _ ->
            model |> withNoCmd


generateNewSection : Cmd Msg
generateNewSection =
    Random.generate NewSection (River.Random.section ClearWater)


generateNewSectionsIfNecessary : Model -> ( Model, Cmd Msg )
generateNewSectionsIfNecessary model =
    case model of
        Playing state ->
            let
                (Feet distance) =
                    Coordinate.distanceBetween state.twinPosition
                        (River.eastEdge state.river)

                needToGenerateDistance =
                    133
            in
                if distance < needToGenerateDistance then
                    ( model, generateNewSection )
                else
                    model |> withNoCmd

        _ ->
            model |> withNoCmd


withNoCmd : a -> ( a, Cmd msg )
withNoCmd value =
    ( value, Cmd.none )


baseRiverFeetPerSecond : Feet
baseRiverFeetPerSecond =
    Feet 25


riverFeetPerSecond : GameState -> Feet
riverFeetPerSecond state =
    ((Measurement.rawFeet <| distanceTravelled state) // 20)
        |> Feet
        |> Measurement.addFeet baseRiverFeetPerSecond


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
            distanceTravelledInInterval (riverFeetPerSecond state) diff

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
            List.map Log.toBoundingBox (River.logs river)

        subject =
            Twins.toBoundingBox twinPosition
    in
        if subject |> Collision.hasCollidedWithAny obstacles then
            Lost state HitObstacle
        else
            Playing state


maxDistanceToWolf : Feet
maxDistanceToWolf =
    Feet 15


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
            Twins.toBoundingBox twinPosition
    in
        (twins |> Collision.hasCrossedHorizontalLine northBank)
            || (twins |> Collision.hasCrossedHorizontalLine southBank)


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


faded : Element -> Element
faded element =
    element
        |> Element.opacity 0.5


introBackground : Element
introBackground =
    Element.image 800 500 "images/tiberinus.jpg"
        |> faded


view : Model -> Html a
view model =
    case model of
        Intro ->
            [ introBackground
            , GameText.intro
            ]
                |> Element.layers
                |> Element.toHtml

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
            , GameText.winScreen (distanceTravelled state)
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
            Renderable.render viewport (Twins.toRenderable state.twinPosition)

        wolves =
            List.map
                (\wolfPos ->
                    Renderable.render viewport (Wolf.toRenderable wolfPos)
                )
                (River.wolves state.river)
                |> Element.layers

        obstacles =
            List.map
                (\logPos ->
                    Renderable.render viewport (Log.toRenderable logPos)
                )
                (River.logs state.river)
                |> Element.layers

        renderedRiver =
            Renderable.positionAt viewport
                (Coordinate.toScreen viewport state.river.position)
                (River.render state.river)

        status =
            GameText.distanceTravelled (distanceTravelled state)
    in
        Element.layers
            [ nature
            , renderedRiver
            , boys
            , obstacles
            , wolves
            , status
            ]


distanceTravelled : GameState -> Feet
distanceTravelled { twinPosition } =
    Coordinate.xDistanceBetween twinPosition initialTwinPosition


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Intro ->
            Mouse.clicks (always StartGame)

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
