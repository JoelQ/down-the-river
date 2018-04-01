module Measurement
    exposing
        ( Feet(..)
        , Pixels(..)
        , pixelsPerFoot
        , pixelsToFeet
        , feetToPixels
        , feetToRawPixels
        , rawFeet
        , addFeet
        , timesUnits
        )


type Feet
    = Feet Int


type Pixels
    = Pixels Int


rawFeet : Feet -> Int
rawFeet (Feet n) =
    n


rawPixels : Pixels -> Int
rawPixels (Pixels n) =
    n


pixelsPerFoot : number
pixelsPerFoot =
    6


pixelsToFeet : Pixels -> Feet
pixelsToFeet (Pixels px) =
    Feet (px // pixelsPerFoot)


feetToPixels : Feet -> Pixels
feetToPixels (Feet ft) =
    Pixels (ft * pixelsPerFoot)


feetToRawPixels : Feet -> Int
feetToRawPixels =
    rawPixels << feetToPixels


addFeet : Feet -> Feet -> Feet
addFeet (Feet f1) (Feet f2) =
    Feet (f1 + f2)


timesUnits : Int -> Feet -> Feet
timesUnits n (Feet f) =
    Feet (n * f)
