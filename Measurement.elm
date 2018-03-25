module Measurement
    exposing
        ( Feet(..)
        , Pixels(..)
        , pixelsPerFoot
        , feetToPixels
        , feetToRawPixels
        , rawFeet
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


feetToPixels : Feet -> Pixels
feetToPixels (Feet ft) =
    Pixels (ft * pixelsPerFoot)


feetToRawPixels : Feet -> Int
feetToRawPixels =
    rawPixels << feetToPixels
