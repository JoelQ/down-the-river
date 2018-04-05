module Lookback
    exposing
        ( Lookback
        , prev
        , value
        , toLookback
        , map
        , mapAnon
        , newValFromPrev
        , prevFromListLast
        , andMap
        , andMapAnon
        , andThen
        , andThenAnon
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


mapAnon : (a -> b) -> Lookback prev a -> Lookback prev b
mapAnon func (Lookback prev value) =
    Lookback prev (func value)


map : (a -> b) -> Lookback prev a -> Lookback a b
map func (Lookback prev value) =
    Lookback value (func value)


newValFromPrev : (prev -> b) -> Lookback prev a -> Lookback prev b
newValFromPrev func (Lookback prev _) =
    Lookback prev (func prev)


prevFromListLast : Lookback a (List a) -> Lookback a (List a)
prevFromListLast (Lookback prev list) =
    case List.reverse list of
        [] ->
            Lookback prev []

        head :: _ ->
            Lookback head list


andMapAnon : Lookback prev2 a -> Lookback prev1 (a -> b) -> Lookback prev2 b
andMapAnon (Lookback prev value) (Lookback _ func) =
    Lookback prev (func value)


andMap : Lookback prev2 a -> Lookback prev1 (a -> b) -> Lookback a b
andMap (Lookback _ value) (Lookback _ func) =
    Lookback value (func value)


andThen : Lookback prev a -> (a -> Lookback a b) -> Lookback a b
andThen (Lookback _ value) func =
    func value


andThenAnon : Lookback prev a -> (a -> Lookback prev b) -> Lookback prev b
andThenAnon (Lookback _ value) func =
    func value
