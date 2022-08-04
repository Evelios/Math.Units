module Units.Interval

open System

open Units

let inline unit<'Unit> : Interval<'Unit> =
    Interval(Quantity LanguagePrimitives.GenericZero, Quantity LanguagePrimitives.GenericOne)

/// Construct an interval from two given values.
///
/// The values should be given in order, but will be swapped if needed to
/// ensure a valid interval is created.
///
///     Interval.from (3, 2)
///     // Interval (2, 3)
let from (first: Quantity<'Unit>) (second: Quantity<'Unit>) : Interval<'Unit> =
    if first <= second then
        Interval(first, second)
    else
        Interval(second, first)


/// Construct an interval from it's endpoints.
///
/// The values should be given in order, but will be swapped if needed to
/// ensure a valid interval is created.
///
///     Interval.fromEndpoints (3, 2)
///     // Interval (2, 3)
let fromEndpoints (first, second) : Interval<'Unit> = from first second

/// Construct a zero width interval containing a single value
let singleton n : Interval<'Unit> = Interval(n, n)


// ---- Accessors ---

/// Get the endpoints of an interval (its minimum and maximum values) as a
/// tuple. The first value will always be less than or equal to the second.
///     ( minValue, maxValue ) =
///         Interval.endpoints someInterval
/// For any interval,
///     Interval.endpoints interval
/// is equivalent to (but more efficient than)
///     ( Interval.minValue interval
///     , Interval.maxValue interval
///     )
let endpoints (Interval.Interval (a, b): Interval<'Unit>) : Quantity<'Unit> * Quantity<'Unit> = (a, b)


/// Get the minimum value of an interval.
///     Interval.minValue (Interval.from 1 3)
///     --> 1
let minValue (Interval (a, _): Interval<'Unit>) : Quantity<'Unit> = a


/// Get the maximum value of an interval.
///     Interval.maxValue (Interval.from 1 3)
///     --> 3
let maxValue (Interval (_, b): Interval<'Unit>) : Quantity<'Unit> = b


/// Get the midpoint of an interval.
///     Interval.midpoint (Interval.from 1 4)
///     --> 2.5
let inline midpoint (Interval (a, b): Interval<'Unit>) : Quantity<'Unit> = a + 0.5 * (b - a)

/// Get the width of an interval.
///     Interval.width (Interval.from 1 5)
///     --> 4
let inline width (Interval (a, b)) : Quantity<'Unit> = b - a

/// Check if the interval is a singleton (the minimum and maximum values are the
/// same).
///     Interval.isSingleton (Interval.from 2 2)
///     --> True
///     Interval.isSingleton (Interval.from 2 3)
///     --> False
let isSingleton (Interval (a, b): Interval<'Unit>) : bool = a = b


// ---- Math Operations ----

/// Construct an interval containing both of the given intervals.
///     firstInterval =
///         Interval.from 1 2
///     secondInterval =
///         Interval.from 3 6
///     Interval.union firstInterval secondInterval
///     --> Interval.from 1 6
/// (Note that this is not strictly speaking a 'union' in the precise mathematical
/// sense, since the result will contain values that are _in between_ the two given
/// intervals and not actually _in_ either of them if those two intervals do not
/// overlap.)
let union (Interval (a1, b1)) (Interval (a2, b2)) : Interval<'Unit> = Interval(min a1 a2, max b1 b2)

/// Attempt to construct an interval containing all the values common to both
/// given intervals. If the intervals do not intersect, returns `Nothing`.
///     Interval.intersection
///         (Interval.from 1 3)
///         (Interval.from 2 5)
///     --> Just (Interval.from 2 3)
///     Interval.intersection
///         (Interval.from 1 3)
///         (Interval.from 4 7)
///     --> Nothing
/// If the two intervals just touch, a singleton interval will be returned:
///     Interval.intersection
///         (Interval.from 1 3)
///         (Interval.from 3 5)
///     --> Just (Interval.singleton 3)
let intersection (Interval (a1, b1)) (Interval (a2, b2)) : Interval<'Unit> option =
    let maxA = max a1 a2
    let minB = min b1 b2

    if maxA <= minB then
        Some(Interval(maxA, minB))
    else
        None

/// Negate an interval. Note that this will flip the order of the endpoints.
///    Interval.negate (Interval.from 2 3)
///    --> Interval.from -3 -2
let inline negate<'Unit when 'Unit: equality and 'Unit: (static member (~-): 'Unit -> 'Unit)>
    (Interval (a, b): Interval<'Unit>)
    : Interval<'Unit> =
    Interval(-b, -a)


/// Add the given amount to an interval.
///    Interval.from -1 5 |> Interval.add 3
///    --> Interval.from 2 8
let inline add (delta: Quantity<'Unit>) (Interval (a, b): Interval<'Unit>) : Interval<'Unit> =
    Interval(delta + a, delta + b)


/// Subtract the given amount from an interval.
///    Interval.from -1 5 |> Interval.subtract 3
///    --> Interval.from -4 2
let inline subtract
    (delta: Quantity<'Unit>)
    (Interval (a: Quantity<'Unit>, b: Quantity<'Unit>): Interval<'Unit>)
    : Interval<'Unit> =
    Interval(a - delta, b - delta)


///  Multiply an interval by a given value. Note that this will flip the order
/// of the interval's endpoints if the given value is negative.
///     Interval.multiplyBy 5 (Interval.from 2 3)
///     --> Interval.from 10 15
///     Interval.multiplyBy -2 (Interval.from 2 3)
///     --> Interval.from -6 -4
let multiplyBy (scale: float) (Interval (a, b): Interval<'Unit>) : Interval<'Unit> =
    if scale >= 0 then
        Interval(a * scale, b * scale)

    else
        Interval(b * scale, a * scale)


/// Divide an interval by a given value. Note that this will flip the order
/// of the interval's endpoints if the given value is negative.
///     Interval.divideBy 2 (Interval.from 2 3)
///     --> Interval.from 1 1.5
///     Interval.divideBy -2 (Interval.from 2 3)
///     --> Interval.from -1.5 -1
let divideBy (divisor: float) (Interval (a, b): Interval<'Unit>) : Interval<'Unit> =
    if divisor = 0. then
        Interval(Quantity(-1. / 0.), Quantity(1. / 0.))

    else if divisor > 0. then
        Interval(a / divisor, b / divisor)

    else
        Interval(b / divisor, a / divisor)


/// Shorthand for `multiplyBy 0.5`.
let half (Interval (a, b): Interval<'Unit>) : Interval<'Unit> = Interval(0.5 * a, 0.5 * b)


/// Shorthand for `multiplyBy 2`.
let twice (Interval (a, b): Interval<'Unit>) : Interval<'Unit> = Interval(2. * a, 2. * b)

/// Add two intervals together.
///     Interval.from 5 10
///         |> Interval.plus (Interval.from 2 3)
///     --> Interval.from 7 13
let plus (Interval (a2, b2)) (Interval (a1, b1)) = Interval(a2 + a1, b2 + b1)


/// Subtract the first interval from the second. This means that `minus` makes
/// the most sense when using `|>`:
///     Interval.from 5 10
///         |> Interval.minus (Interval.from 2 3)
///     --> Interval.from 2 8
/// Without the pipe operator, the above would be written as:
///     Interval.minus (Interval.from 2 3)
///         (Interval.from 5 10)
///     --> Interval.from 2 8
let minus (Interval (a2, b2)) (Interval (a1, b1)) = Interval(a1 - b2, b1 - a2)


/// Multiply the two given intervals.
///     Interval.from 10 12
///         |> Interval.times
///             (Interval.from 5 6)
///     --> Interval.from 50 72
let times (Interval (a2, b2)) (Interval (a1, b1)) =
    let aa = a1 * a2

    let ab = a1 * b2

    let ba = b1 * a2

    let bb = b1 * b2
    Interval(min (min (min aa ab) ba) bb, max (max (max aa ab) ba) bb)

/// The maximum of cos(x) is x = 2 pi \* k for every integer k.
/// If `minValue` and `maxValue` are in different branches
/// (meaning different values of k), then the interval must pass through
/// 2 pi \* k, which means the interval must include the maximum value.
let cosIncludesMax (Interval (a, b)) : bool =
    let minBranch = floor (a / (2. * Math.PI))

    let maxBranch = floor (b / (2. * Math.PI))
    minBranch <> maxBranch

/// cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
/// that means cos(interval) includes the minimum.
let cosIncludesMinMax (interval: Interval<'Unit>) : bool * bool =
    (interval
     |> add (Quantity Math.PI)
     |> cosIncludesMax,
     interval |> cosIncludesMax)

/// cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
/// the maximum/minimum, that means sin(interval) includes the maximum/minimum
/// accordingly.
let sinIncludesMinMax (interval: Interval<'Unit>) : bool * bool =
    let halfPi = Math.PI / 2.

    interval
    |> subtract (Quantity halfPi)
    |> cosIncludesMinMax


/// Get the image of sin(x) applied on the interval.
///     Interval.sin (Interval.from 0 (degrees 45))
///     --> Interval.from 0 0.7071
///     Interval.sin (Interval.from 0 pi)
///     --> Interval.from 0 1
let sin (interval: Interval<Radians>) : Interval<Unitless> =
    if isSingleton interval then
        singleton (Angle.sin (minValue interval) |> Quantity)

    else
        let includesMin, includesMax =
            sinIncludesMinMax interval

        let a, b = endpoints interval

        let newMin =
            if includesMin then
                -1.

            else
                min (Angle.sin a) (Angle.sin b)

        let newMax =
            if includesMax then
                1.

            else
                max (Angle.sin a) (Angle.sin b)

        fromEndpoints (Quantity newMin, Quantity newMax)


///  Get the image of cos(x) applied on the interval.
///     Interval.cos (Interval.from 0 (degrees 45))
///     --> Interval.from 0.7071 1
///     Interval.cos (Interval.from 0 pi)
///     --> Interval.from -1 1
let cos (interval: Interval<Radians>) : Interval<Unitless> =
    if isSingleton interval then
        singleton (Angle.cos (minValue interval) |> Quantity)

    else
        let includesMin, includesMax =
            cosIncludesMinMax interval

        let a, b = endpoints interval

        let newMin =
            if includesMin then
                -1.

            else
                min (Angle.cos a) (Angle.cos b)

        let newMax =
            if includesMax then
                1.

            else
                max (Angle.cos a) (Angle.cos b)

        fromEndpoints (Quantity newMin, Quantity newMax)


// ---- Queries ----

/// Interpolate between an interval's endpoints based on a parameter value that
/// will generally be between 0.0 and 1.0. A value of 0.0 corresponds to the minimum
/// value of the interval, a value of 0.5 corresponds to its midpoint and a value of
/// 1.0 corresponds to its maximum value:
///     Interval.interpolate (Interval.from 1 5) 0
///     --> 1
///     Interval.interpolate (Interval.from 1 5) 0.75
///     --> 4
/// Values less than 0.0 or greater than 1.0 can be used to extrapolate:
///     Interval.interpolate (Interval.from 1 5) 1.5
///     --> 7
/// Note that because of how [`Interval.from`](#from) works, the interpolation is in
/// fact from the minimum value to the maximum, _not_ "from the first
/// `Interval.from` argument to the second":
///     Interval.interpolate (Interval.from 0 10) 0.2
///     --> 2
///     Interval.interpolate (Interval.from 10 0) 0.2
///     --> 2 -- not 8!
/// If you want the interpolate from one number down to another, you can use
/// [`Float.Extra.interpolateFrom`](https://package.elm-lang.org/packages/ianmackenzie/elm-float-extra/latest/Float-Extra#interpolateFrom)
/// from the `elm-float-extra` package.
let interpolate (Interval (a, b): Interval<'Unit>) (t: float) : Quantity<'Unit> =
    interpolateFrom a.Value b.Value t |> Quantity


/// Given an interval and a given value, determine the corresponding
/// interpolation parameter (the parameter that you would pass to [`interpolate`](#interpolate)
/// to get the given value):
///     Interval.interpolationParameter
///         (Interval.from 10 15)
///         12
///     --> 0.4
/// The result will be between 0 and 1 if (and only if) the given value is within
/// the given interval:
///     Interval.interpolationParameter
///         (Interval.from 10 15)
///         18
///     --> 1.6
///     Interval.interpolationParameter
///         (Interval.from 10 15)
///         9
///     --> -0.2
/// This is the inverse of `interpolate`; for any non-zero-width `interval`,
///     Interval.interpolationParameter interval value
///         |> Interval.interpolate interval
/// should be equal to the original `value` (within numerical round off).
let interpolationParameter (Interval (a, b): Interval<'Unit>) (value: Quantity<'Unit>) : float =
    if a < b then
        (value - a) / (b - a)

    else

    if value < a then
        -1. / 0.

    else if value > b then
        1. / 0.

    else
        // value, a and intervalMaxValue are all equal
        0.

/// Test if a value is contained with a particular interval.
let contains (value: Quantity<'Unit>) (Interval (a, b): Interval<'Unit>) : bool = a <= value && value <= b


/// Check if two intervals touch or overlap (have any values in common).
///     Interval.from -5 5
///         |> Interval.intersects (Interval.from 0 10)
///     --> True
///     Interval.from -5 5
///         |> Interval.intersects (Interval.from 10 20)
///     --> False
/// Intervals that just touch each other are considered to intersect (this is
/// consistent with `intersection` which will return a zero-width interval for the
/// intersection of two just-touching intervals):
///     Interval.from -5 5
///         |> Interval.intersects (Interval.from 5 10)
///     --> True
let intersects (Interval (a1, b1): Interval<'Unit>) (Interval (a2, b2): Interval<'Unit>) : bool = a1 <= b2 && b1 >= a2


/// Check if the second interval is fully contained in the first.
///     Interval.from -5 5
///         |> Interval.isContainedIn (Interval.from 0 10)
///     --> False
///     Interval.from -5 5
///         |> Interval.isContainedIn (Interval.from -10 10)
///     --> True
/// Be careful with the argument order! If not using the `|>` operator, the second
/// example would be written as:
///     Interval.isContainedIn (Interval.from -10 10)
///         (Interval.from -5 5)
///     --> True
let isContainedIn (Interval (a1, b1): Interval<'Unit>) (Interval (a2, b2): Interval<'Unit>) : bool =
    a1 <= a2 && b2 <= b1



/// Find the interval containing one or more input values:
///    Interval.hull 5 [ 3, 2, 4 ]
///    --> Interval.from 2 5
/// See also [`hullN`](#hullN).
let hull (first: 'Unit) (rest: 'Unit list) : Interval<'Unit> =
    let rec hullHelp a b values =
        match values with
        | value :: rest -> hullHelp (min a value) (max b value) rest

        | [] -> Interval(a, b)

    hullHelp first first rest

/// Construct an interval containing the three given values;
///     Interval.hull3 a b c
/// is equivalent to
///     Interval.hull a [ b, c ]
/// but is more efficient. (If you're looking for a `hull2` function, [`from`](#from)
/// should do what you want.)
let hull3 (a: 'Unit) (b: 'Unit) (c: 'Unit) : Interval<'Unit> =
    Interval(min a (min b c), max a (max b c))

/// Attempt to construct an interval containing all _N_ values in the given
/// list. If the list is empty, returns `Nothing`. If you know you have at least one
/// value, you can use [`hull`](#hull) instead.
///     Interval.hullN [ 2, 1, 3 ]
///     --> Just (Interval.from 1 3)
///     Interval.hullN [ -3 ]
///     --> Just (Interval.singleton -3)
///     Interval.hullN []
///     --> Nothing
let hullN (values: 'Unit list) : Interval<'Unit> option =
    match values with
    | first :: rest -> Some(hull first rest)
    | [] -> None

/// Like [`hull`](#hull), but lets you work on any kind of item as long as a
/// number can be extracted from it. For example, if you had
///     type alias Person =
///         { name : String
///         , age : Float
///         }
/// then given some people you could find their range of ages as an `Interval`
/// using
///     Interval.hullOf .age
///         firstPerson
///         [ secondPerson
///         , thirdPerson
///         , fourthPerson
///         ]
/// See also [`hullOfN`](#hullOfN).
let hullOf (getValue: 'a -> 'Unit) (first: 'a) (rest: 'a list) : Interval<'Unit> =
    let rec hullOfHelp a b getValue list =
        match list with
        | first :: rest ->
            let value = getValue first
            hullOfHelp (min a value) (max b value) getValue rest

        | [] -> Interval(a, b)

    let firstValue = getValue first
    hullOfHelp firstValue firstValue getValue rest

/// Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
let hullOfN (getValue: 'a -> 'Unit) (items: 'a list) : Interval<'Unit> option =
    match items with
    | first :: rest -> Some(hullOf getValue first rest)
    | [] -> None

/// Construct an interval containing one or more given intervals:
///     Interval.aggregate
///         (Interval.singleton 2)
///         [ Interval.singleton 4
///         , Interval.singleton 3
///         ]
///     --> Interval.from 2 4
/// Works much like [`hull`](#hull). See also [`aggregateN`](#aggregateN).
let aggregate (Interval (a, b)) (rest: Interval<'Unit> list) : Interval<'Unit> =
    let rec aggregateHelp a b intervals =
        match intervals with
        | Interval (c, d) :: rest -> aggregateHelp (min a c) (max b d) rest

        | [] -> Interval(a, b)

    aggregateHelp a b rest

/// Special case of [`aggregate`](#aggregate) for the case of three intervals;
///     Interval.aggregate3 first second third
/// is equivalent to
///     Interval.aggregate first [ second, third ]
/// but is more efficient. (If you're looking for an `aggregate2` function,
/// [`union`](#union) should do what you want.)
let aggregate3 (Interval (a1, b1)) (Interval (a2, b2)) (Interval (a3, b3)) : Interval<'Unit> =
    Interval(min a1 (min a2 a3), max b1 (max b2 b3))

///  Attempt to construct an interval containing all of the intervals in the given
/// list. If the list is empty, returns `Nothing`. If you know you have at least one
/// interval, you can use [`aggregate`](#aggregate) instead.
let aggregateN intervals : Interval<'Unit> option =
    match intervals with
    | first :: rest -> Some(aggregate first rest)
    | [] -> None

/// Like [`aggregate`](#aggregate), but lets you work on any kind of item as
/// long as an interval can be generated from it (similar to [`hullOf`](#hullOf)).
let aggregateOf (getInterval: 'a -> Interval<'Unit>) (first: 'a) (rest: 'a list) : Interval<'Unit> =
    let rec aggregateOfHelp a b getInterval items =
        match items with
        | first :: rest ->
            let (Interval (c, d)) = getInterval first
            aggregateOfHelp (min a c) (max b d) getInterval rest

        | [] -> Interval(a, b)

    let (Interval (a, b)) = getInterval first
    aggregateOfHelp a b getInterval rest

/// Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
let aggregateOfN (getInterval: 'a -> Interval<'Unit>) (items: 'a list) : Interval<'Unit> option =
    match items with
    | first :: rest -> Some(aggregateOf getInterval first rest)
    | [] -> None
