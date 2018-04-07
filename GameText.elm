module GameText exposing (strandedOnShoreScreen, hitObstacleScreen, winScreen)

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
        |> Element.flow Element.down



-- CONTENT


winScreen : Element
winScreen =
    [ header "You win!", standardText winMessage ]
        |> display


hitObstacleScreen : Element
hitObstacleScreen =
    [ header "You lost!", standardText hitObstacleMessage ]
        |> display


strandedOnShoreScreen : Element
strandedOnShoreScreen =
    [ header "You lost!", standardText strandedOnShoreMessage ]
        |> display


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
