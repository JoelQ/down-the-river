module GameText
    exposing
        ( strandedOnShoreScreen
        , hitObstacleScreen
        , winScreen
        , intro
        )

import Element exposing (Element)
import Text exposing (defaultStyle)


-- UTILITY


header : String -> Element
header string =
    string
        |> Text.fromString
        |> Text.style headerStyle
        |> Element.centered


standardText : String -> Element
standardText string =
    string
        |> Text.fromString
        |> Text.style textStyle
        |> Element.centered


headerStyle : Text.Style
headerStyle =
    { defaultStyle
        | height = Just 55
        , typeface = [ "Helvetica", "Arial", "sans-serif" ]
    }


textStyle : Text.Style
textStyle =
    { defaultStyle
        | height = Just 25
        , typeface = [ "Helvetica", "Arial", "sans-serif" ]
    }



-- LAYOUT


display : List Element -> Element
display texts =
    texts
        |> List.map centerHorizontally
        |> Element.flow Element.down


centerHorizontally : Element -> Element
centerHorizontally element =
    Element.container 800 (Element.heightOf element) Element.middle element



-- CONTENT


intro : Element
intro =
    display
        [ standardText introMessage
        , standardText startMessage
        ]


winScreen : Element
winScreen =
    display
        [ header "You win!"
        , standardText winMessage
        , standardText restartMessage
        ]


hitObstacleScreen : Element
hitObstacleScreen =
    display
        [ header "You lost!"
        , standardText hitObstacleMessage
        , standardText restartMessage
        ]


strandedOnShoreScreen : Element
strandedOnShoreScreen =
    display
        [ header "You lost!"
        , standardText strandedOnShoreMessage
        , standardText restartMessage
        ]


introMessage : String
introMessage =
    """
    You are Tiberinus, spirit of the river Tiber.

    The evil king Amalius hopes to get rid of his nephews Romulus and
    Remus by setting the twins adrift on the river in a basket.
    Surely some terrible accident will befall them.

    Not so fast! You decide to act. Guide the basket around the obstacles.
    The twins need care and nurture, perhaps a friendly wolf...
    """


winMessage : String
winMessage =
    """
  The wolf takes in the twins and cares for them. The gods must destine
  them for greatness!
  """


hitObstacleMessage : String
hitObstacleMessage =
    """
  Your fragile basket hits an obstacle in the river, drowning the twins.
  It seems the omens where wrong this time. These boys will never
  amount to much.
  """


strandedOnShoreMessage : String
strandedOnShoreMessage =
    """
  Your basket washes up on shore. Unfortunately no one notices,
  sealing the twins' fate. Amulius' nefarious plan has succeeded.
  """


restartMessage : String
restartMessage =
    "Click anywhere to play again..."


startMessage : String
startMessage =
    "Click anywhere to start playing..."
