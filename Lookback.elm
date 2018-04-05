module Lookback
    exposing
        ( Lookback
        , prev
        , value
        , toLookback
        , map
        , andMap
        , andMapPrev
        , andThen
        )


type Lookback prev value
    = Lookback prev value


toLookback : prev -> a -> Lookback prev a
toLookback prev a =
    Lookback prev a


prev : Lookback prev a -> prev
prev (Lookback p _) =
    p


value : Lookback prev a -> a
value (Lookback _ val) =
    val


map : (a -> b) -> Lookback prev a -> Lookback prev b
map func (Lookback prev value) =
    Lookback prev (func value)


andMap : Lookback prev a -> Lookback prev (a -> b) -> Lookback prev b
andMap (Lookback _ value) (Lookback prev func) =
    Lookback prev (func value)


andMapPrev : Lookback prev a -> Lookback prev (a -> b) -> Lookback a b
andMapPrev (Lookback _ value) (Lookback _ func) =
    Lookback value (func value)


andThen : Lookback prev a -> (a -> Lookback prev b) -> Lookback prev b
andThen (Lookback _ value) func =
    func value
