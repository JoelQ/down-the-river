module River
    exposing
        ( River
        , accross
        , render
        , initial
        , logs
        , wolves
        , eastEdge
        , appendSection
        )

import Collage
import Color
import Coordinate
import Element exposing (Element)
import Measurement exposing (Feet(..), addFeet, timesUnits)
import Section exposing (Section, Block)


type alias River =
    { position : Coordinate.World
    , sections : List Section
    }


initial : River
initial =
    { position = Coordinate.world 0 30
    , sections = [ Section.empty, Section.first ]
    }


appendSection : Section -> River -> River
appendSection section river =
    { river | sections = river.sections ++ [ section ] }


logs : River -> List Coordinate.World
logs river =
    river
        |> blocks
        |> List.indexedMap Section.obstaclePositions
        |> List.filterMap identity
        |> List.concat


wolves : River -> List Coordinate.World
wolves river =
    river
        |> blocks
        |> List.indexedMap Section.wolfPosition
        |> List.filterMap identity


blocks : River -> List Block
blocks { sections } =
    List.concatMap Section.blocks sections


accross : Feet
accross =
    Feet 30


eastEdge : River -> Coordinate.World
eastEdge river =
    let
        (Feet x) =
            riverWidth river
    in
        river.position
            |> Coordinate.moveBy (toFloat x) 0


riverWidth : River -> Feet
riverWidth { sections } =
    sections
        |> List.map Section.width
        |> List.foldl addFeet (Feet 0)


render : River -> Element
render river =
    let
        height =
            Measurement.feetToRawPixels accross

        width =
            Measurement.feetToRawPixels (riverWidth river)
    in
        Collage.rect (toFloat width) (toFloat height)
            |> Collage.filled Color.blue
            |> List.singleton
            |> Collage.collage width height
