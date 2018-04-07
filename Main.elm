module Main exposing (main)

import AnimationFrame
import Collage
import Color
import Coordinate exposing (Viewport)
import Element exposing (Element)
import Game exposing (Game(..), YDirection(..), LossReason(..))
import GameText
import Html exposing (Html)
import Keyboard
import Log
import Measurement exposing (Feet(..), Pixels(..))
import Mouse
import Random
import Renderable
import River exposing (River)
import River.Random
import Section exposing (ObstacleArrangement(..), Section)
import Time exposing (Time)
import Twins
import Wolf


type alias Model =
    Game


type Msg
    = Tick Time
    | Move Game.YDirection
    | NewSection Section
    | StartGame


init : ( Model, Cmd Msg )
init =
    ( Game.intro, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            Game.tick diff model
                |> generateNewSectionsIfNecessary

        Move direction ->
            Game.move direction model
                |> withNoCmd

        NewSection section ->
            Game.appendNewSection section model
                |> withNoCmd

        StartGame ->
            Game.start model
                |> withNoCmd


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
            , GameText.winScreen (Game.distanceTravelled state)
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


viewGameState : Game.State -> Element
viewGameState state =
    let
        viewport =
            Game.viewportFor state.twinPosition

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
            GameText.distanceTravelled (Game.distanceTravelled state)
    in
        Element.layers
            [ nature
            , renderedRiver
            , boys
            , obstacles
            , wolves
            , status
            ]


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
