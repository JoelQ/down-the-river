module Game
    exposing
        ( Game(..)
        , YDirection(..)
        , LossReason(..)
        , State
        , intro
        , start
        , move
        , appendNewSection
        , distanceTravelled
        , tick
        , viewportFor
        )

import Collision
import Coordinate exposing (Viewport)
import Log
import Measurement exposing (Feet(..), Pixels(..))
import River exposing (River)
import Section exposing (Section)
import Time exposing (Time)
import Twins


type Game
    = Intro
    | Playing State
    | Lost State LossReason
    | Won State


type LossReason
    = HitObstacle
    | StrandedOnShore


type YDirection
    = Up
    | Down
    | Drifting


type alias State =
    { river : River
    , twinPosition : Coordinate.World
    , yDirection : YDirection
    }


intro : Game
intro =
    Intro


startState : State
startState =
    { twinPosition = initialTwinPosition
    , river = River.initial
    , yDirection = Drifting
    }


initialTwinPosition : Coordinate.World
initialTwinPosition =
    let
        (Feet width) =
            viewportWidth
                |> Measurement.pixelsToFeet
    in
        Coordinate.world (toFloat <| width // 2) 41



-- VIEWPORT


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



-- GAME TRANSITIONS


start : Game -> Game
start game =
    case game of
        Intro ->
            Playing startState

        Lost _ _ ->
            Playing startState

        Won _ ->
            Playing startState

        Playing _ ->
            game


appendNewSection : Section -> Game -> Game
appendNewSection section game =
    case game of
        Playing state ->
            { state | river = River.appendSection section state.river }
                |> Playing

        _ ->
            game


move : YDirection -> Game -> Game
move direction game =
    case game of
        Playing state ->
            { state | yDirection = direction }
                |> Playing

        _ ->
            game


tick : Time -> Game -> Game
tick diff game =
    game
        |> map (moveTwinsDownstream diff)
        |> andThen checkLoseCondition
        |> andThen checkArrivalOnBank



-- STATE TRANSITIONS


moveTwinsDownstream : Time -> State -> State
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


checkLoseCondition : State -> Game
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


checkArrivalOnBank : State -> Game
checkArrivalOnBank state =
    if hasArrivedOnBank state then
        if closeEnoughToWolf state then
            Won state
        else
            Lost state StrandedOnShore
    else
        Playing state


baseRiverFeetPerSecond : Feet
baseRiverFeetPerSecond =
    Feet 25


riverFeetPerSecond : State -> Feet
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


hasArrivedOnBank : State -> Bool
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


maxDistanceToWolf : Feet
maxDistanceToWolf =
    Feet 15


closeEnoughToWolf : State -> Bool
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



-- UTILITY


distanceTravelled : State -> Feet
distanceTravelled { twinPosition } =
    Coordinate.xDistanceBetween twinPosition initialTwinPosition



-- MONOMORPHIC FUNCTOR & MONAD??!!


map : (State -> State) -> Game -> Game
map func game =
    case game of
        Playing state ->
            Playing (func state)

        _ ->
            game


andThen : (State -> Game) -> Game -> Game
andThen func game =
    case game of
        Playing state ->
            func state

        _ ->
            game
