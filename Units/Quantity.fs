[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Quantity

open System

/// A generic zero value. This can be treated as either an `Int` or `Float`
/// quantity in any units type, similar to how `Nothing` can be treated as any kind
/// of `Maybe` type and `[]` can be treated as any kind of `List`.
let zero: Quantity<'Units> =
    Quantity LanguagePrimitives.GenericZero


/// A generic positive infinity value.
let positiveInfinity:  Quantity<'Units> =
    Quantity System.Double.PositiveInfinity
        
        
/// Alias for `positiveInfinity`.
let infinity : Quantity<'Units> =
    positiveInfinity


/// A generic negative infinity value.
let negativeInfinity:  Quantity<'Units> =
    Quantity System.Double.NegativeInfinity



// --- COMPARISON --------------------------------------------------------------


/// Check if one quantity is less than another. Note the [argument order](/#argument-order)!
///    oneMeter =
///        Length.meters 1
///    Length.feet 1 |> Quantity.lessThan oneMeter
///    --> True
///    -- Same as:
///    Quantity.lessThan oneMeter (Length.feet 1)
///    --> True
///    List.filter (Quantity.lessThan oneMeter)
///        [ Length.feet 1
///        , Length.parsecs 1
///        , Length.yards 1
///        , Length.lightYears 1
///        ]
///    --> [ Length.feet 1, Length.yards 1 ]
let lessThan (y:Quantity<'Units> ) (x: Quantity<'Units> ):  bool =
    x < y


/// Check if one quantity is greater than another. Note the [argument order](/#argument-order)!
///    oneMeter =
///        Length.meters 1
///    Length.feet 1 |> Quantity.greaterThan oneMeter
///    --> False
///    -- Same as:
///    Quantity.greaterThan oneMeter (Length.feet 1)
///    --> False
///    List.filter (Quantity.greaterThan oneMeter)
///        [ Length.feet 1
///        , Length.parsecs 1
///        , Length.yards 1
///        , Length.lightYears 1
///        ]
///    --> [ Length.parsecs 1, Length.lightYears 1 ]
let greaterThan (y: Quantity<'Units>) (x: Quantity<'Units>): bool =
    x > y


/// Check if one quantity is less than or equal to another. Note the [argument
/// order](/#argument-order)!
let lessThanOrEqualTo (y: Quantity<'Units>) (x: Quantity<'Units>): bool =
    x <= y


/// Check if one quantity is greater than or equal to another. Note the
/// [argument order](/#argument-order)!
let greaterThanOrEqualTo (y: Quantity<'Units>) (x: Quantity<'Units>): bool =
    x >= y


/// Short form for `Quantity.lessThan Quantity.zero`.
let lessThanZero (x: Quantity<'Units>): bool =
    x < Quantity LanguagePrimitives.GenericZero


/// Short form for `Quantity.greaterThan Quantity.zero`.
let greaterThanZero (x: Quantity<'Units>): bool =
    x > Quantity LanguagePrimitives.GenericZero


/// Short form for `Quantity.lessThanOrEqualTo Quantity.zero`.
let lessThanOrEqualToZero (x: Quantity<'Units>): bool =
    x <= Quantity LanguagePrimitives.GenericZero


/// Short form for `Quantity.greaterThanOrEqualTo Quantity.zero`.
let greaterThanOrEqualToZero (x: Quantity<'Units>): bool =
    x >= Quantity LanguagePrimitives.GenericZero


/// Compare two quantities, returning an [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
/// value indicating whether the first is less than, equal to or greater than the
/// second.
///     Quantity.compare
///         (Duration.minutes 90)
///         (Duration.hours 1)
///     --> GT
///     Quantity.compare
///         (Duration.minutes 60)
///         (Duration.hours 1)
///     --> EQ
let compare (x: Quantity<'Units>) (y: Quantity<'Units>): int =
    x.Comparison(y)
    
/// Get the absolute value of a quantity.
///    Quantity.abs (Duration.milliseconds -10)
///    --> Duration.milliseconds 10
///
/// This function can be called from the global function or the module function
///    // Using the default globally included function
///    Microsoft.FSharp.Core.Operators.abs quantity
///    abs quantity  // This function is included by default in F#
/// 
///    open Units
///    Units.abs quantity
///    
let abs (quantity: Quantity<'Units>): Quantity<'Units> = abs quantity


/// Check if two quantities are equal within a given absolute tolerance. The
/// given tolerance must be greater than or equal to zero - if it is negative, then
/// the result will always be false.
///     -- 3 feet is 91.44 centimeters or 0.9144 meters
///     Quantity.equalWithin (Length.centimeters 10)
///         (Length.meters 1)
///         (Length.feet 3)
///     --> True
///     Quantity.equalWithin (Length.centimeters 5)
///         (Length.meters 1)
///         (Length.feet 3)
///     --> False
let equalWithin (tolerance: Quantity<'Units>) (x: Quantity<'Units>) (y: Quantity<'Units>): bool =
    abs (x - y) <= tolerance


/// Find the maximum of two quantities.
///    Quantity.max (Duration.hours 2) (Duration.minutes 100)
///    --> Duration.hours 2
let max (x: Quantity<'Units>) (y: Quantity<'Units>): Quantity<'Units> = max x y
    


/// Find the minimum of two quantities.
///    Quantity.min (Duration.hours 2) (Duration.minutes 100)
///    --> Duration.minutes 100
let min (x: Quantity<'Units>) (y: Quantity<'Units>) : Quantity<'Units>= min x y


/// Check if a quantity is positive or negative infinity.
///    Quantity.isInfinite
///        (Length.meters 1
///            |> Quantity.per (Duration.seconds 0)
///        )
///    --> True
///    Quantity.isInfinite Quantity.negativeInfinity
///    --> True
let isInfinite (Quantity value: Quantity<'Units>): bool =
    System.Double.IsInfinity value


/// Check if a quantity's underlying value is NaN (not-a-number).
///    Quantity.isNan (Quantity.sqrt (Area.squareMeters -4))
///    --> True
///    Quantity.isNan (Quantity.sqrt (Area.squareMeters 4))
///    --> False
let isNaN (Quantity value: Quantity<'Units>): bool =
    System.Double.IsNaN value



// ---- ARITHMETIC -------------------------------------------------------------


/// Negate a quantity!
//    Quantity.negate (Length.millimeters 10)
//    --> Length.millimeters -10
let negate (value: Quantity<'Units>): Quantity<'Units> =
    -value


/// Add two quantities.
///    Length.meters 1 |> Quantity.plus (Length.centimeters 5)
///    --> Length.centimeters 105
let plus (y: Quantity<'Units>) (x: Quantity<'Units>): Quantity<'Units>  =
    x + y


/// Subtract one quantity from another.
///    Quantity.difference
///        (Duration.hours 1)
///        (Duration.minutes 15)
///    --> Duration.minutes 45
let difference (x: Quantity<'Units>) (y: Quantity<'Units>): Quantity<'Units> =
    x - y


/// An 'infix' version of [`difference`](#difference), intended to be used in
/// pipeline form;
///     Quantity.difference x y
/// can be written as
///     x |> Quantity.minus y
/// Note that unlike `difference`, this also means that partial application will 'do
/// the right thing':
///     List.map (Quantity.minus fifteenMinutes)
///         [ Duration.hours 1
///         , Duration.hours 2
///         , Duration.minutes 30
///         ]
///     --> [ Duration.minutes 45
///     --> , Duration.minutes 105
///     --> , Duration.minutes 15
///     --> ]
let minus (y: Quantity<'Units>) (x: Quantity<'Units>) : Quantity<'Units>=
    x - y


/// Multiply two quantities with units types `units1` and `units2` together,
/// resulting in a quantity with units type `Product units1 units2`.
/// This works for any two units types, but one special case is worth pointing out.
/// The units type of an [`Area`](Area) is `SquareMeters`, which is a type alias for
/// `Squared Meters`, which in turn expands to `Product Meters Meters`. This means
/// that the product of two `Length`s does in fact give you an `Area`:
///     -- This is the definition of an acre, I kid you not 😈
///     Quantity.product (Length.feet 66) (Length.feet 660)
///     --> Area.acres 1
/// We can also multiply an `Area` by a `Length` to get a `Volume`:
///     Quantity.product
///         (Area.squareMeters 1)
///         (Length.centimers 1)
///     --> Volume.liters 10
/// Note that there are [other forms of multiplication](/#multiplication-and-division)!

let product (x: Quantity<'Units>) (y: Quantity<'Units>) =
    x * y


/// An 'infix' version of [`product`](#product), intended to be used in pipeline
/// form;
///     Quantity.product a b
/// can be written as
///     a |> Quantity.times b
let times (y: Quantity<'Units>) (x: Quantity<'Units>) =
    x * y


/// If you use [`times`](#times) or [`product`](#product) to multiply one
/// quantity by another [unitless](#Unitless) quantity, for example
///     quantity |> Quantity.times unitlessQuantity
/// then the result you'll get will have units type `Product units Unitless`. But
/// this is silly and not super useful, since the product of `units` and `Unitless`
/// should really just be `units`. That's what `timesUnitless` does - it's a special
/// case of `times` for when you're multiplying by another unitless quantity, that
/// leaves the units alone.
/// You can think of `timesUnitless` as shorthand for `toFloat` and `multiplyBy`;
/// for `Float`-valued quantities,
///     quantity |> Quantity.timesUnitless unitlessQuantity
/// is equivalent to
///     quantity
///         |> Quantity.multiplyBy
///             (Quantity.toFloat unitlessQuantity)
let timesUnitless (Quantity y: Quantity<Unitless>) (Quantity x: Quantity<Unitless>): Quantity<Unitless> =
    Quantity (x * y)


/// Divide a quantity in `Product units1 units2` by a quantity in `units1`,
/// resulting in another quantity in `units2`. For example, the units type of a
/// `Force` is `Product Kilograms MetersPerSecondSquared` (mass times acceleration),
/// so we could divide a force by a given mass to determine how fast that mass would
/// be accelerated by the given force:
///     Force.newtons 100
///         |> Quantity.over
///             (Mass.kilograms 50)
///     --> Acceleration.metersPerSecondSquared 2
/// Note that there are [other forms of division](/#multiplication-and-division)!
/// 
/// over : Quantity Float units1 -> Quantity Float (Product units1 units2) -> Quantity Float units2
/// over (Quantity y) (Quantity x) =
///     Quantity (x / y)


/// Just like `over` but divide by a quantity in `units2`, resulting in another
/// quantity in `units1`. For example, we could divide a force by a desired
/// acceleration to determine how much mass could be accelerated at that rate:
///     Force.newtons 100
///         |> Quantity.over_
///             (Acceleration.metersPerSecondSquared 5)
///     --> Mass.kilograms 20
let over_ (y: Quantity<'Units>) (x: Quantity<'Units>) =
    x / y


/// Similar to [`timesUnitless`](#timesUnitless), `overUnitless` lets you
/// divide one quantity by a second [unitless](#Unitless) quantity without affecting
/// the units;
///     quantity |> Quantity.overUnitless unitlessQuantity
/// is equivalent to
///     quantity
///         |> Quantity.divideBy
///             (Quantity.toFloat unitlessQuantity)
let overUnitless (y: Quantity<'Units>) (x: Quantity<'Units>) =
    x / y


/// Find the ratio of two quantities with the same units.
//    Quantity.ratio (Length.miles 1) (Length.yards 1)
//    --> 1760
let ratio (x: Quantity<'Units>) (y: Quantity<'Units>) =
    x / y


/// Scale a `Quantity` by a `number`.
///     Quantity.multiplyBy 1.5 (Duration.hours 1)
///     --> Duration.minutes 90
/// Note that there are [other forms of multiplication](/#multiplication-and-division)!
let multiplyBy scale (Quantity value) =
    Quantity (scale * value)


/// Divide a `Quantity` by a `Float`.
///     Quantity.divideBy 2 (Duration.hours 1)
///     --> Duration.minutes 30
/// Note that there are [other forms of division](/#multiplication-and-division)!
let divideBy divisor (Quantity value) =
    Quantity (value / divisor)


/// Convenient shorthand for `Quantity.multiplyBy 2`.
///    Quantity.twice (Duration.minutes 30)
///    --> Duration.hours 1
let twice (value: Quantity<'Units>): Quantity<'Units> =
    2. * value


/// Convenient shorthand for `Quantity.multiplyBy 0.5`.
///    Quantity.half (Length.meters 1)
///    --> Length.centimeters 50
let half (value: Quantity<'Units>): Quantity<'Units> =
    0.5 * value


/// Given a lower and upper bound, clamp a given quantity to within those
/// bounds. Say you wanted to clamp an angle to be between +/-30 degrees:
///     lowerBound =
///         Angle.degrees -30
///     upperBound =
///         Angle.degrees 30
///     Quantity.clamp lowerBound upperBound (Angle.degrees 5)
///     --> Angle.degrees 5
///     -- One radian is approximately 57 degrees
///     Quantity.clamp lowerBound upperBound (Angle.radians 1)
///     --> Angle.degrees 30
///     Quantity.clamp lowerBound upperBound (Angle.turns -0.5)
///     --> Angle.degrees -30
//let clamp (Quantity lower) (Quantity upper) (Quantity value) =
//    if lower <= upper then
//        Quantity (Math.Clamp(lower,upper,value))
//
//    else
//        Quantity ((Math.Clamp(upper,lower,value))


// TODO: Get rid of NaN
/// Get the sign of a quantity. This will return 1, -1, 0 or NaN if the given
/// quantity is positive, negative, zero or NaN respectively.
///     Quantity.sign (Length.meters 3)
///     --> 1
///     Quantity.sign (Length.meters -3)
///     --> -1
///     Quantity.sign (Length.meters 0)
///     --> 0
///     Quantity.sign Quantity.positiveInfinity
///     --> 1
///     Quantity.sign (Length.meters (0 / 0))
///     --> NaN
let sign (value: Quantity<'Units>): int =
    if greaterThanZero value then
        1

    else if lessThanZero value then
        -1

    else if isNaN value then
        0

    else
        0


/// Square a quantity with some `units`, resulting in a new quantity in
/// `Squared units`:
///     Quantity.squared (Length.meters 5)
///     --> Area.squareMeters 25
/// See also [`squaredUnitless`](#squaredUnitless).
let squared (value: Quantity<'Units>): Quantity<'Units Squared> =
    value * value


/// 
let squaredUnitless (Quantity value: Quantity<Unitless>): Quantity<Unitless> =
    Quantity (value * value)

/// 
let sqrtUnitless (Quantity value: Quantity<Unitless>): Quantity<Unitless> =
    Quantity (sqrt value)
    
/// Take a quantity in `Squared units` and return the square root of that
/// quantity in plain `units`:
///     Quantity.sqrt (Area.hectares 1)
///     --> Length.meters 100
/// Getting fancier, you could write a 2D hypotenuse (magnitude) function that
/// worked on _any_ quantity type (length, speed, force...) as
///     hypotenuse :
///         Quantity Float units
///         -> Quantity Float units
///         -> Quantity Float units
///     hypotenuse x y =
///         Quantity.sqrt
///             (Quantity.squared x
///                 |> Quantity.plus
///                     (Quantity.squared y)
///             )
/// This works because:
///   - The `x` and `y` arguments are both in `units`
///   - So each squared item is in `Squared units`
///   - So the sum is also in `Squared units`
///   - And calling `sqrt` on something in `Squared units` returns a value back in
///     `units`
/// See also [`sqrtUnitless`](#sqrtUnitless).
let sqrt (Quantity value : Quantity<'Units Squared>) = Quantity(sqrt value)


/// Cube a quantity with some `units`, resulting in a new quantity in
/// `Cubed units`.
///     Quantity.cubed (Length.meters 5)
///     --> Volume.cubicMeters 125
/// See also [`cubedUnitless`](#cubedUnitless).
let cubed (Quantity value: Quantity<'Units>): Quantity<'Units Cubed> =
    Quantity (value * value * value)


/// 
let cubedUnitless (Quantity value: Quantity<Unitless>): Quantity<Unitless> =
    Quantity (value * value * value)


let unsafeCbrt (Quantity value: Quantity<'Units>): Quantity<'Units> =
    if value >= 0. then
        Quantity (Math.Pow(value, (1. / 3.)))

    else
        Quantity -(Math.Pow(-value, (1. / 3.)))


/// Take a quantity in `Cubed units` and return the cube root of that
/// quantity in plain `units`.
///     Quantity.cbrt (Volume.liters 1)
///     --> Length.centimeters 10
/// See also [`cbrtUnitless`](#cbrtUnitless).
let cbrt quantity =
    unsafeCbrt quantity


/// 
let cbrtUnitless quantity =
    unsafeCbrt quantity


/// Find the inverse of a unitless value.
///    Quantity.reciprocal (Quantity.float 5)
///    --> Quantity.float 0.2
let reciprocal (Quantity value: Quantity<'Units>): Quantity<'Units> =
    Quantity (1. / value)


// TODO: may be incorrect
/// 
let modBy (modulus: Quantity<'Units>) (value: Quantity<'Units>): Quantity<'Units> =
    value % modulus


/// 
let fractionalModBy (Quantity modulus) (Quantity value) =
    Quantity (value - modulus * floor (value / modulus))


// TODO: may be incorrect
/// 
let remainderBy (modulus: Quantity<'Units>) (value: Quantity<'Units>): Quantity<'Units> =
    value % modulus


/// 
let fractionalRemainderBy (Quantity modulus) (Quantity value) =
    Quantity (value - modulus * floor (value / modulus))


/// Interpolate from the first quantity to the second, based on a parameter that
/// ranges from zero to one. Passing a parameter value of zero will return the start
/// value and passing a parameter value of one will return the end value.
///     fiveMeters =
///         Length.meters 5
///     tenMeters =
///         Length.meters 10
///     Quantity.interpolateFrom fiveMeters tenMeters 0
///     --> Length.meters 5
///     Quantity.interpolateFrom fiveMeters tenMeters 1
///     --> Length.meters 10
///     Quantity.interpolateFrom fiveMeters tenMeters 0.6
///     --> Length.meters 8
/// The end value can be less than the start value:
///     Quantity.interpolateFrom tenMeters fiveMeters 0.1
///     --> Length.meters 9.5
/// Parameter values less than zero or greater than one can be used to extrapolate:
///     Quantity.interpolateFrom fiveMeters tenMeters 1.5
///     --> Length.meters 12.5
///     Quantity.interpolateFrom fiveMeters tenMeters -0.5
///     --> Length.meters 2.5
///     Quantity.interpolateFrom tenMeters fiveMeters -0.2
///     --> Length.meters 11
let interpolateFrom (Quantity start: Quantity<'Units>) (Quantity finish: Quantity<'Units>) (Quantity parameter: Quantity<'Units>): Quantity<'Units> =
    if parameter <= 0.5 then
        Quantity (start + parameter * (finish - start))

    else
        Quantity (finish + (1. - parameter) * (start - finish))


/// Find the midpoint between two quantities.
///    Quantity.midpoint (Length.meters 5) (Length.meters 10)
///    --> Length.meters 7.5
let midpoint (x: Quantity<'Units>) (y: Quantity<'Units>): Quantity<'Units> =
    x + 0.5 * (y - x)


/// Construct a range of evenly-spaced values given a `start` value, an `end`
/// value and the number of `steps` to take from the start to the end. The first
/// value in the returned list will be equal to `start` and the last value will be
/// equal to `end`. Note that the number of returned values will be one greater than
/// the number of steps!
///     Quantity.range
///         { start = Length.meters 2
///         , end = Length.meters 3
///         , steps = 5
///         }
///     --> [ Length.centimeters 200
///     --> , Length.centimeters 220
///     --> , Length.centimeters 240
///     --> , Length.centimeters 260
///     --> , Length.centimeters 280
///     --> , Length.centimeters 300
///     --> ]
/// The start and end values can be in either order:
///     Quantity.range
///         { start = Duration.hours 1
///         , end = Quantity.zero
///         , steps = 4
///         }
///     --> [ Duration.minutes 60
///     --> , Duration.minutes 45
///     --> , Duration.minutes 30
///     --> , Duration.minutes 15
///     --> , Duration.minutes 0
///     --> ]
/// Passing a negative or zero `steps` value will result in an empty list being
/// returned.
/// If you need finer control over what values get generated, try combining
/// `interpolateFrom` with the various functions in the
/// [`elm-1d-parameter`](https://package.elm-lang.org/packages/ianmackenzie/elm-1d-parameter/latest/)
/// package. For example:
///     -- Same as using Quantity.range
///     Parameter1d.steps 4 <|
///         Quantity.interpolateFrom
///             (Length.meters 2)
///             (Length.meters 3)
///     --> [ Length.centimeters 200
///     --> , Length.centimeters 225
///     --> , Length.centimeters 250
///     --> , Length.centimeters 275
///     --> , Length.centimeters 300
///     --> ]
///     -- Omit the last value
///     Parameter1d.leading 4 <|
///         Quantity.interpolateFrom
///             (Length.meters 2)
///             (Length.meters 3)
///     --> [ Length.centimeters 200
///     --> , Length.centimeters 225
///     --> , Length.centimeters 250
///     --> , Length.centimeters 275
///     --> ]
let range (start: Quantity<'Units>) (finish: Quantity<'Units>) (steps: int): Quantity<'Units> list  =
    let rec rangeHelp start finish (i: int) (steps: float) (accumulatedValues: Quantity<'Units> list): Quantity<'Units> list =
        let value =
                interpolateFrom start finish (Quantity( float i / steps))

        let updatedValues =
                value :: accumulatedValues
                
        if i = 0 then
            updatedValues

        else
            rangeHelp start finish (i - 1) steps updatedValues
            
    if steps > 0 then
        rangeHelp start finish steps (float steps) []

    else
        []


/// Generalized units conversion function that lets you convert to many kinds of
/// units not directly supported by `elm-units`. The first argument is a function
/// that constructs a value of the desired unit type, and the second is the quantity
/// to convert. For example,
///     Speed.metersPerSecond 5
///         |> Speed.inFeetPerSecond
///     --> 16.4042
/// is equivalent to
///     Speed.metersPerSecond 5
///         |> Quantity.in_ Speed.feetPerSecond
///     --> 16.4042
/// More interestingly, if you wanted to get speed in some weirder unit like
/// millimeters per minute (not directly supported by `elm-units`), you could do
///     Speed.metersPerSecond 5
///         |> Quantity.in_
///             (Length.millimeters
///                 >> Quantity.per (Duration.minutes 1)
///             )
///     --> 300000
/// Internally,
///     Quantity.in_ someUnits someQuantity
/// is simply implemented as
///     Quantity.ratio someQuantity (someUnits 1)
let in_ units quantity =
    ratio quantity (units 1)


// ---- INT/FLOAT CONVERSIONS --------------------------------------------------


/// Round a `Float`-valued quantity to the nearest `Int`. Note that [this may
/// not do what you expect](#-int-float-conversion).
///     Quantity.round (Pixels.pixels 3.5)
///     --> Pixels.pixels 4
let round (value: Quantity<'Units>): Quantity<'Units> = round value


/// Round a `Float`-valued quantity down to the nearest `Int`. Note that [this
/// may not do what you expect](#-int-float-conversion).
///     Quantity.floor (Pixels.pixels 2.9)
///     --> Pixels.pixels 2
///     Quantity.floor (Pixels.pixels -2.1)
///     --> Pixels.pixels -3
let floor (value: Quantity<'Units>): Quantity<'Units> = floor value


/// Round a `Float`-valued quantity up to the nearest `Int`. Note that [this may
/// not do what you expect](#-int-float-conversion).
///     Quantity.ceiling (Pixels.pixels 1.2)
///     --> Pixels.pixels 2
///     Quantity.ceiling (Pixels.pixels -2.1)
///     --> Pixels.pixels -2
let ceil (value: Quantity<'Units>): Quantity<'Units> = ceil value


/// Round a `Float`-valued quantity towards zero. Note that [this may not do
/// what you expect](#-int-float-conversion).
///     Quantity.truncate (Pixels.pixels -2.8)
///     --> Pixels.pixels -2
let truncate (Quantity value) =
    Quantity (Basics.truncate value)


// TODO: need `Quantity<Int, 'Units>`
/// Convert a `Quantity Int units` to a `Quantity Float units` with the same
/// value. Useful when you have an `Int`-valued quantity and want to divide it by
/// something, multiply it by a fractional value etc.
let toFloatQuantity (Quantity value: Quantity<'Units>): Quantity<'Units> =
    Quantity (float value)


// ---- LIST FUNCTIONS ---------------------------------------------------------


/// Find the sum of a list of quantities.
///    Quantity.sum
///        [ Length.meters 1
///        , Length.centimeters 2
///        , Length.millimeters 3
///        ]
///    --> Length.meters 1.023
///    Quantity.sum []
///    --> Quantity.zero
let sum quantities =
    List.foldl plus zero quantities


/// Find the minimum value in a list of quantities. Returns `Nothing` if the
/// list is empty.
///    Quantity.minimum
///        [ Mass.kilograms 1
///        , Mass.pounds 2
///        , Mass.tonnes 3
///        ]
///    --> Just (Mass.pounds 2)
let minimum quantities =
    case quantities of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl min first rest)


/// Find the maximum value in a list of quantities. Returns `Nothing` if the
/// list is empty.
///     Quantity.maximum
///         [ Mass.kilograms 1
///         , Mass.pounds 2
///         , Mass.tonnes 3
///         ]
///     --> Just (Mass.tonnes 3)
let maximum quantities =
    case quantities of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl max first rest)


/// Find the 'minimum' item in a list as measured by some derived `Quantity`:
///     people =
///         [ { name = "Bob", height = Length.meters 1.6 }
///         , { name = "Charlie", height = Length.meters 2.0 }
///         , { name = "Alice", height = Length.meters 1.8 }
///         ]
///     Quantity.minimumBy .height people
///     --> Just { name = "Bob", height = Length.meters 1.6 }
/// If the list is empty, returns `Nothing`. If multiple items in the list are tied,
/// then the first one is returned.
let minimumBy toQuantity quantities =
    let minimumByHelp toQuantity currentItem currentValue quantities =
        case quantities of
            [] ->
                currentItem

            item :: rest ->
                let
                    (Quantity value) =
                        toQuantity item
                in
                if value < currentValue then
                    minimumByHelp toQuantity item value rest

                else
                    minimumByHelp toQuantity currentItem currentValue rest
    
    case quantities of
        [] ->
            Nothing

        firstItem :: rest ->
            let
                (Quantity firstValue) =
                    toQuantity firstItem
            in
            Just (minimumByHelp toQuantity firstItem firstValue rest)




/// Find the 'maximum' item in a list as measured by some derived `Quantity`:
///     people =
///         [ { name = "Bob", height = Length.meters 1.6 }
///         , { name = "Charlie", height = Length.meters 2.0 }
///         , { name = "Alice", height = Length.meters 1.8 }
///         ]
///     Quantity.maximumBy .height people
///     --> Just { name = "Charlie", height = Length.meters 2.0 }
/// If the list is empty, returns `Nothing`. If multiple items in the list are tied,
/// then the first one is returned.
let maximumBy toQuantity quantities =
    let maximumByHelp toQuantity currentItem currentValue quantities =
        case quantities of
            [] ->
                currentItem

            item :: rest ->
                let
                    (Quantity value) =
                        toQuantity item
                in
                if value > currentValue then
                    maximumByHelp toQuantity item value rest

                else
                    maximumByHelp toQuantity currentItem currentValue rest
                    
    case quantities of
        [] ->
            Nothing

        firstItem :: rest ->
            let
                (Quantity firstValue) =
                    toQuantity firstItem
            in
            Just (maximumByHelp toQuantity firstItem firstValue rest)




/// Sort a list of quantities.
///    Quantity.sort
///        [ Mass.kilograms 1
///        , Mass.pounds 2
///        , Mass.tonnes 3
///        ]
///    --> [ Mass.pounds 2
///    --> , Mass.kilograms 1
///    --> , Mass.tonnes 3
///    --> ]
let sort quantities =
    List.sortBy unwrap quantities


/// Sort an arbitrary list of values by a derived `Quantity`. If you had
///     people =
///         [ { name = "Bob", height = Length.meters 1.6 }
///         , { name = "Charlie", height = Length.meters 2.0 }
///         , { name = "Alice", height = Length.meters 1.8 }
///         ]
/// then you could sort by name with
///     List.sortBy .name people
///     --> [ { name = "Alice", height = Length.meters 1.8 }
///     --> , { name = "Bob", height = Length.meters 1.6 }
///     --> , { name = "Charlie", height = Length.meters 2.0 }
///     --> ]
/// (nothing new there!), and sort by height with
///     Quantity.sortBy .height people
///     --> [ { name = "Bob", height = Length.meters 1.6 }
///     --> , { name = "Alice", height = Length.meters 1.8 }
///     --> , { name = "Charlie", height = Length.meters 2.0 }
///     --> ]
let sortBy toQuantity list =
    let
        comparator first second =
            compare (toQuantity first) (toQuantity second)
    in
    List.sortWith comparator list



// ---- WORKING WITH RATES -----------------------------------------------------


/// Construct a rate of change by dividing a dependent quantity (numerator) by
/// an independent quantity (denominator):
///     speed =
///         Quantity.rate (Length.miles 1) Duration.minute
///     speed |> Speed.inMilesPerHour
///     --> 60
/// Note that we could directly use our rate of change value as a `Speed`! That is
/// because many built-in quantity types are defined as rates of change, for
/// example:
///   - `Speed` is `Length` per `Duration`
///   - `Acceleration` is `Speed` per `Duration`
///   - `Pressure` is `Force` per `Area`
///   - `Power` is `Energy` per `Duration`
///   - `Current` is `Charge` per `Duration`
///   - `Resistance` is `Voltage` per `Current`
///   - `Voltage` is `Power` per `Current`
/// Note that there are [other forms of division](/#multiplication-and-division)!
/// 
let rate (Quantity dependentValue) (Quantity independentValue) =
    Quantity (dependentValue / independentValue)
    
/// 'Infix' version of [`rate`](#rate), meant to be used in pipeline form;
///     Quantity.rate distance time
/// can be written as
///     distance |> Quantity.per time
/// 
let per (Quantity independentValue) (Quantity dependentValue) =
 Quantity (dependentValue / independentValue)
 
/// Multiply a rate of change by an independent quantity (the denominator in
/// the rate) to get a total value:
///     Duration.minutes 30
///         |> Quantity.at
///             (Speed.kilometersPerHour 100)
///     --> Length.kilometers 50
/// Can be useful to define conversion functions from one unit to another, since
/// if you define a `rate` then `Quantity.at rate` will give you a conversion
/// function:
///     pixelDensity : Quantity Float (Rate Pixels Meters)
///     pixelDensity =
///         Pixels.pixels 96 |> Quantity.per (Length.inches 1)
///     lengthToPixels : Length -> Quantity Float Pixels
///     lengthToPixels length =
///         Quantity.at pixelDensity length
///     lengthToPixels (Length.inches 3)
///     --> Pixels.pixels 288
/// Eagle-eyed readers will note that using partial application you could also
/// simply write
///     lengthToPixels =
///         Quantity.at pixelDensity
/// Note that there are [other forms of multiplication](/#multiplication-and-division)!
/// 
let at (Quantity rateOfChange) (Quantity independentValue) =
 Quantity (rateOfChange * independentValue)
 
/// Given a rate and a _dependent_ quantity (total value), determine the
/// necessary amount of the _independent_ quantity:
///     Length.kilometers 75
///         |> Quantity.at_
///             (Speed.kilometersPerHour 100)
///     --> Duration.minutes 45
/// Where `at` performs multiplication, `at_` performs division - you multiply a
/// speed by a duration to get a distance, but you divide a distance by a speed to
/// get a duration.
/// Similar to `at`, `at_` can be used to define an _inverse_ conversion function:
///     pixelDensity : Quantity Float (Rate Pixels Meters)
///     pixelDensity =
///         Pixels.pixels 96 |> Quantity.per (Length.inches 1)
///     pixelsToLength : Quantity Float Pixels -> Length
///     pixelsToLength pixels =
///         Quantity.at_ pixelDensity pixels
///     pixelsToLength (Pixels.pixels 48)
///     --> Length.inches 0.5
let at_ (Quantity rateOfChange) (Quantity dependentValue) =
    Quantity (dependentValue / rateOfChange)


/// Same as `at` but with the argument order flipped, which may read better
/// in some cases:
///     Speed.kilometersPerHour 100
///         |> Quantity.for
///             (Duration.minutes 30)
///     --> Length.kilometers 50
let for_ (Quantity independentValue) (Quantity rateOfChange) =
    Quantity (rateOfChange * independentValue)


/// Find the inverse of a given rate. May be useful if you are using a rate to
/// define a conversion, and want to convert the other way;
///     Quantity.at (Quantity.inverse rate)
/// is equivalent to
///     Quantity.at_ rate
let inverse (Quantity rateOfChange) =
     Quantity (1 / rateOfChange)


/// Multiply two rates of change that 'cancel out' together, resulting in a new
/// rate. For example, if you know the real-world speed of an on-screen object and
/// the display resolution, then you can get the speed in pixels per second:
///     realWorldSpeed =
///         Speed.metersPerSecond 0.1
///     resolution =
///         Pixels.float 96 |> Quantity.per Length.inch
///     Quantity.rateProduct realWorldSpeed resolution
///     --> Pixels.pixelsPerSecond 377.95
/// That is, "length per duration" multiplyed by "pixels per length" gives you
/// "pixels per duration".
/// Sometimes you can't directly multiply two rates to get what you want, in which
/// case you may need to use [`inverse`](#inverse) in combination with
/// `rateProduct`. For example, if you know the on-screen speed of some object and
/// the display resolution, then you can use those to get the real-world speed:
///     pixelSpeed =
///         Pixels.pixelsPerSecond 500
///     resolution =
///         Pixels.float 96 |> Quantity.per Length.inch
///     Quantity.rateProduct pixelSpeed
///         (Quantity.inverse resolution)
///     --> Speed.metersPerSecond 0.1323
let rateProduct (Quantity firstRate) (Quantity secondRate) =
    Quantity (firstRate * secondRate)



/// ---- UNITLESS QUANTITIES ---------------------------------------------------
/// Convert a plain `Int` into a `Quantity Int Unitless` value.
let int value =
    Quantity value


/// Convert a `Quantity Int Unitless` value into a plain `Int`.
let toInt (Quantity value) =
    value


/// Convert a plain `Float` into a `Quantity Float Unitless` value.
let float value =
    Quantity value


/// Convert a `Quantity Float Unitless` value into a plain `Float`.
/// If you're looking for a function to convert a `Quantity Int units` to `Quantity
/// Float units`, check out [`toFloatQuantity`](#toFloatQuantity).
let toFloat (Quantity value) =
    value


/// 
let unsafe value =
    Quantity value


/// 
let unwrap (Quantity value) =
    value

