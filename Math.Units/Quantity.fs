[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Quantity

open System

/// <summary>
/// Quantity of any floating point value. This structure is represented as a
/// class so that all these functions can be inherited by the type alias.
/// This allows easy access to all of the base quantity functionality without
/// having to write any wrapper functions. This is achieved by creating a type alias,
/// in this example using a length or distance represented in meters <c>type
/// Length = Quantity<Meters></c>. If you would like to extend the <c>Length</c> type you can
/// just create a <c>Length</c> module and add any extension functions there.
/// </summary>
type Quantity<'Units> with

    /// <summary>
    /// Create a unitless quantity. Unitless quantities must use the unitless
    /// functions in this module to avoid accumulating unit ratios. Using
    /// the functions within this module maintains the <c>Unitless</c> type.
    /// </summary>
    static member unitless value : Quantity<Unitless> = Quantity value

    /// <summary>
    /// A generic zero value. This can be treated as a quantity in any
    /// units type, similar to how <c>Nothing</c> can be treated as any kind
    /// of <c>Maybe</c> type and <c>[]</c> can be treated as any kind of <c>List</c>.
    /// </summary>
    static member zero: Quantity<'Units> =
        Quantity LanguagePrimitives.GenericZero


    /// A generic positive infinity value.
    static member positiveInfinity: Quantity<'Units> =
        Quantity Double.PositiveInfinity


    /// <summary>
    /// Alias for <c>positiveInfinity</c>.
    /// </summary>
    static member infinity: Quantity<'Units> =
        Quantity.positiveInfinity


    /// <summary>
    /// A generic negative infinity value.
    /// </summary>
    static member negativeInfinity: Quantity<'Units> =
        Quantity Double.NegativeInfinity

    // ---- Unsafe Operations ------------------------------------------------------

    /// This function allows you to create a quantity of any value and type.
    /// This should only try to use in library functions. This does however
    /// let you create units of generic types and types that are compiler
    /// defined instead of user defined.
    static member create<'Units> value : Quantity<'Units> = Quantity value


    /// This function provides access to the floating point value represented by
    /// the quantity. This should only try to use in library functions.
    static member unwrap(quantity: Quantity<'Units>) : float = quantity.Value



    // --- Comparison --------------------------------------------------------------


    /// <summary>
    /// Check if one quantity is less than another. Note the <b>argument order!</b>
    ///
    /// <example>
    /// <code>
    ///     let oneMeter =
    ///         Length.meters 1
    /// 
    ///     Length.feet 1 |> Quantity.lessThan oneMeter
    ///     --> True
    ///
    ///     // Is the same as:
    ///     Quantity.lessThan oneMeter (Length.feet 1)
    ///     --> True
    ///
    /// </code>
    /// </example>
    ///
    /// <example>
    /// <code>
    ///     List.filter (Quantity.lessThan oneMeter)
    ///         [ Length.feet 1
    ///           Length.parsecs 1
    ///           Length.yards 1
    ///           Length.lightYears 1
    ///         ]
    ///     --> [ Length.feet 1; Length.yards 1 ]
    /// </code>
    /// </example>
    /// </summary>
    static member lessThan (y: Quantity<'Units>) (x: Quantity<'Units>) : bool = x < y


    /// Check if one quantity is greater than another. Note the <b>argument order!</b>
    /// <example><code>
    ///    oneMeter =
    ///        Length.meters 1
    ///    Length.feet 1 |> Quantity.greaterThan oneMeter
    ///    // --> False
    /// 
    ///    // Same as:
    ///    Quantity.greaterThan oneMeter (Length.feet 1)
    ///    // --> False
    ///    List.filter (Quantity.greaterThan oneMeter)
    ///        [ Length.feet 1
    ///        , Length.parsecs 1
    ///        , Length.yards 1
    ///        , Length.lightYears 1
    ///        ]
    ///    --> [ Length.parsecs 1, Length.lightYears 1 ]
    /// </code></example>
    static member greaterThan (y: Quantity<'Units>) (x: Quantity<'Units>) : bool = x > y


    /// Check if one quantity is less than or equal to another. Note the [argument
    /// order](/#argument-order)!
    static member lessThanOrEqualTo (y: Quantity<'Units>) (x: Quantity<'Units>) : bool = x <= y


    /// Check if one quantity is greater than or equal to another. Note the
    /// [argument order](/#argument-order)!
    static member greaterThanOrEqualTo (y: Quantity<'Units>) (x: Quantity<'Units>) : bool = x >= y


    /// Short form for <c>Quantity.lessThan Quantity.zero</c>.
    static member lessThanZero(x: Quantity<'Units>) : bool =
        x < Quantity LanguagePrimitives.GenericZero


    /// Short form for <c>Quantity.greaterThan Quantity.zero</c>.
    static member greaterThanZero(x: Quantity<'Units>) : bool =
        x > Quantity LanguagePrimitives.GenericZero


    /// Short form for <c>Quantity.lessThanOrEqualTo Quantity.zero</c>.
    static member lessThanOrEqualToZero(x: Quantity<'Units>) : bool =
        x <= Quantity LanguagePrimitives.GenericZero


    /// Short form for <c>Quantity.greaterThanOrEqualTo Quantity.zero</c>.
    static member greaterThanOrEqualToZero(x: Quantity<'Units>) : bool =
        x >= Quantity LanguagePrimitives.GenericZero


    /// Compare two quantities, returning an [<c>Order</c>](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
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
    static member compare (x: Quantity<'Units>) (y: Quantity<'Units>) : int = x.Comparison(y)

    /// <summary>
    ///     Get the absolute value of a quantity.
    /// </summary>
    ///
    /// <example>
    ///   <code>
    ///     Quantity.abs (Duration.milliseconds -10)
    ///     Duration.milliseconds 10
    ///   </code>
    /// </example>
    ///
    /// <example>
    ///   This function can be called from the global function or the module
    ///   function. They both return the same result
    ///   <code>
    ///     abs quantity
    ///     Units.abs quantity
    ///   </code>
    /// </example>
    static member abs(quantity: Quantity<'Units>) : Quantity<'Units> = Quantity.Abs quantity


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
    static member equalWithin (tolerance: Quantity<'Units>) (x: Quantity<'Units>) (y: Quantity<'Units>) : bool =
        abs (x - y) <= abs tolerance


    /// Find the maximum of two quantities.
    ///    Quantity.max (Duration.hours 2) (Duration.minutes 100)
    ///    --> Duration.hours 2
    static member max (x: Quantity<'Units>) (y: Quantity<'Units>) : Quantity<'Units> = max x y



    /// Find the minimum of two quantities.
    ///    Quantity.min (Duration.hours 2) (Duration.minutes 100)
    ///    --> Duration.minutes 100
    static member min (x: Quantity<'Units>) (y: Quantity<'Units>) : Quantity<'Units> = min x y


    /// Check if a quantity is positive or negative infinity.
    ///    Quantity.isInfinite
    ///        (Length.meters 1
    ///            |> Quantity.per (Duration.seconds 0)
    ///        )
    ///    --> True
    ///    Quantity.isInfinite Quantity.negativeInfinity
    ///    --> True
    static member isInfinite(quantity: Quantity<'Units>) : bool = Double.IsInfinity quantity.Value


    /// Check if a quantity's underlying value is NaN (not-a-number).
    ///    Quantity.isNan (Quantity.sqrt (Area.squareMeters -4))
    ///    --> True
    ///    Quantity.isNan (Quantity.sqrt (Area.squareMeters 4))
    ///    --> False
    static member isNaN(quantity: Quantity<'Units>) : bool = Double.IsNaN quantity.Value



    // ---- Arithmetic -------------------------------------------------------------


    /// Negate a quantity!
    //    Quantity.negate (Length.millimeters 10)
    //    --> Length.millimeters -10
    static member negate(value: Quantity<'Units>) : Quantity<'Units> = -value


    /// Add two quantities.
    ///    Length.meters 1 |> Quantity.plus (Length.centimeters 5)
    ///    --> Length.centimeters 105
    static member plus (y: Quantity<'Units>) (x: Quantity<'Units>) : Quantity<'Units> = x + y


    /// Subtract one quantity from another.
    ///    Quantity.difference
    ///        (Duration.hours 1)
    ///        (Duration.minutes 15)
    ///    --> Duration.minutes 45
    static member difference (x: Quantity<'Units>) (y: Quantity<'Units>) : Quantity<'Units> = x - y


    /// An 'infix' version of [<c>difference</c>](#difference), intended to be used in
    /// pipeline form;
    ///     Quantity.difference x y
    /// can be written as
    ///     x |> Quantity.minus y
    /// Note that unlike <c>difference</c>, this also means that partial application will 'do
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
    static member minus (y: Quantity<'Units>) (x: Quantity<'Units>) : Quantity<'Units> = x - y


    /// Multiply two quantities with units types <c>units1</c> and <c>units2</c> together,
    /// resulting in a quantity with units type <c>Product units1 units2</c>.
    /// This works for any two units types, but one special case is worth pointing out.
    /// The units type of an [<c>Area</c>](Area) is <c>SquareMeters</c>, which is a type alias for
    /// <c>Squared Meters</c>, which in turn expands to <c>Product Meters Meters</c>. This means
    /// that the product of two <c>Length</c>s does in fact give you an <c>Area</c>:
    ///     -- This is the definition of an acre, I kid you not 😈
    ///     Quantity.product (Length.feet 66) (Length.feet 660)
    ///     --> Area.acres 1
    /// We can also multiply an <c>Area</c> by a <c>Length</c> to get a <c>Volume</c>:
    ///     Quantity.product
    ///         (Area.squareMeters 1)
    ///         (Length.centimeters 1)
    ///     --> Volume.liters 10
    /// Note that there are [other forms of multiplication](/#multiplication-and-division)!

    static member product (x: Quantity<'Units>) (y: Quantity<'Units>) = x * y


    /// An 'infix' version of [<c>product</c>](#product), intended to be used in pipeline
    /// form;
    ///     Quantity.product a b
    /// can be written as
    ///     a |> Quantity.times b
    static member times (y: Quantity<'Units>) (x: Quantity<'Units>) = x * y


    /// If you use [<c>times</c>](#times) or [<c>product</c>](#product) to multiply one
    /// quantity by another [unitless](#Unitless) quantity, for example
    ///     quantity |> Quantity.times unitlessQuantity
    /// then the result you'll get will have units type <c>Product units Unitless</c>. But
    /// this is silly and not super useful, since the product of <c>units</c> and <c>Unitless</c>
    /// should really just be <c>units</c>. That's what <c>timesUnitless</c> does - it's a special
    /// case of <c>times</c> for when you're multiplying by another unitless quantity, that
    /// leaves the units alone.
    /// You can think of <c>timesUnitless</c> as shorthand for <c>toFloat</c> and <c>multiplyBy</c>;
    /// for <c>Float</c>-valued quantities,
    ///     quantity |> Quantity.timesUnitless unitlessQuantity
    /// is equivalent to
    ///     quantity
    ///         |> Quantity.multiplyBy
    ///             (Quantity.toFloat unitlessQuantity)
    static member timesUnitless (y: Quantity<Unitless>) (x: Quantity<Unitless>) : Quantity<Unitless> =
        Quantity(x.Value * y.Value)


    /// Divide a quantity in <c>Product units1 units2</c> by a quantity in <c>units1</c>,
    /// resulting in another quantity in <c>units2</c>. For example, the units type of a
    /// <c>Force</c> is <c>Product Kilograms MetersPerSecondSquared</c> (mass times acceleration),
    /// so we could divide a force by a given mass to determine how fast that mass would
    /// be accelerated by the given force:
    ///     Force.newtons 100
    ///         |> Quantity.over
    ///             (Mass.kilograms 50)
    ///     --> Acceleration.metersPerSecondSquared 2
    /// Note that there are [other forms of division](/#multiplication-and-division)!
    ///
    static member over (y: Quantity<'U1>) (x: Quantity<Product<'U1, 'U2>>) : Quantity<'U2> = Quantity(x.Value / y.Value)


    /// Just like <c>over</c> but divide by a quantity in <c>units2</c>, resulting in another
    /// quantity in <c>units1</c>. For example, we could divide a force by a desired
    /// acceleration to determine how much mass could be accelerated at that rate:
    ///     Force.newtons 100
    ///         |> Quantity.over_
    ///             (Acceleration.metersPerSecondSquared 5)
    ///     --> Mass.kilograms 20
    static member over_ (y: Quantity<'U2>) (x: Quantity<Product<'U1, 'U2>>) : Quantity<'U1> =
        Quantity(x.Value / y.Value)


    /// Similar to [<c>timesUnitless</c>](#timesUnitless), <c>overUnitless</c> lets you
    /// divide one quantity by a second [unitless](#Unitless) quantity without affecting
    /// the units;
    ///     quantity |> Quantity.overUnitless unitlessQuantity
    /// is equivalent to
    ///     quantity
    ///         |> Quantity.divideBy
    ///             (Quantity.toFloat unitlessQuantity)
    static member overUnitless (y: Quantity<Unitless>) (x: Quantity<Unitless>) : Quantity<Unitless> = Quantity(x / y)


    /// Find the ratio of two quantities with the same units.
    //    Quantity.ratio (Length.miles 1) (Length.yards 1)
    //    --> 1760
    static member ratio (x: Quantity<'Units>) (y: Quantity<'Units>) : float = x / y


    /// Scale a <c>Quantity</c> by a <c>number</c>.
    ///     Quantity.multiplyBy 1.5 (Duration.hours 1)
    ///     --> Duration.minutes 90
    /// Note that there are [other forms of multiplication](/#multiplication-and-division)!
    static member multiplyBy (scale: float) (quantity: Quantity<'Units>) : Quantity<'Units> =
        Quantity(scale * quantity.Value)


    /// Divide a <c>Quantity</c> by a <c>Float</c>.
    ///     Quantity.divideBy 2 (Duration.hours 1)
    ///     --> Duration.minutes 30
    /// Note that there are [other forms of division](/#multiplication-and-division)!
    static member divideBy (divisor: float) (quantity: Quantity<'Units>) : Quantity<'Units> =
        Quantity(quantity.Value / divisor)


    /// Convenient shorthand for <c>Quantity.multiplyBy 2</c>.
    ///    Quantity.twice (Duration.minutes 30)
    ///    --> Duration.hours 1
    static member twice(quantity: Quantity<'Units>) : Quantity<'Units> = 2. * quantity


    /// Convenient shorthand for <c>Quantity.multiplyBy 0.5</c>.
    ///    Quantity.half (Length.meters 1)
    ///    --> Length.centimeters 50
    static member half(quantity: Quantity<'Units>) : Quantity<'Units> = 0.5 * quantity


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
    static member clamp (lower: Quantity<'Units>) (upper: Quantity<'Units>) (quantity: Quantity<'Units>) =
        let clampHelper l u =
            if quantity.Value < lower.Value then
                l
            else if quantity.Value > upper.Value then
                u
            else
                quantity

        if lower <= upper then
            clampHelper lower upper
        else
            clampHelper upper lower

    /// Square a quantity with some <c>units</c>, resulting in a new quantity in
    /// <c>Squared units</c>:
    ///     Quantity.squared (Length.meters 5)
    ///     --> Area.squareMeters 25
    /// See also [<c>squaredUnitless</c>](#squaredUnitless).
    static member squared(quantity: Quantity<'Units>) : Quantity<'Units Squared> = quantity * quantity


    ///
    static member squaredUnitless(quantity: Quantity<Unitless>) : Quantity<Unitless> =
        Quantity(quantity.Value * quantity.Value)

    ///
    static member sqrtUnitless(quantity: Quantity<Unitless>) : Quantity<Unitless> = Quantity(sqrt quantity.Value)

    /// Take a quantity in <c>Squared units</c> and return the square root of that
    /// quantity in plain <c>units</c>:
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
    ///   - The <c>x</c> and <c>y</c> arguments are both in <c>units</c>
    ///   - So each squared item is in <c>Squared units</c>
    ///   - So the sum is also in <c>Squared units</c>
    ///   - And calling <c>sqrt</c> on something in <c>Squared units</c> returns a value back in
    ///     <c>units</c>
    /// See also [<c>sqrtUnitless</c>](#sqrtUnitless).
    static member sqrt(quantity: Quantity<'Units Squared>) = Quantity(sqrt quantity.Value)


    /// Cube a quantity with some <c>units</c>, resulting in a new quantity in
    /// <c>Cubed units</c>.
    ///     Quantity.cubed (Length.meters 5)
    ///     --> Volume.cubicMeters 125
    /// See also [<c>cubedUnitless</c>](#cubedUnitless).
    static member cubed(quantity: Quantity<'Units>) : Quantity<'Units Cubed> =
        Quantity(quantity.Value * quantity.Value * quantity.Value)


    ///
    static member cubedUnitless(quantity: Quantity<Unitless>) : Quantity<Unitless> =
        Quantity(quantity.Value * quantity.Value * quantity.Value)


    static member unsafeCbrt(quantity: Quantity<'Units1>) : Quantity<'Units2> =
        if quantity.Value >= 0. then
            Quantity(Math.Pow(quantity.Value, (1. / 3.)))

        else
            Quantity(-(Math.Pow(-quantity.Value, (1. / 3.))))


    /// Take a quantity in <c>Cubed units</c> and return the cube root of that
    /// quantity in plain <c>units</c>.
    ///     Quantity.cbrt (Volume.liters 1)
    ///     --> Length.centimeters 10
    /// See also [<c>cbrtUnitless</c>](#cbrtUnitless).
    static member cbrt(quantity: Quantity<'Units Cubed>) : Quantity<'Units> = Quantity.unsafeCbrt quantity


    ///
    static member cbrtUnitless(quantity: Quantity<Unitless>) : Quantity<Unitless> = Quantity.unsafeCbrt quantity


    /// Find the inverse of a unitless quantity.
    ///    Quantity.reciprocal (Quantity.float 5)
    ///    --> Quantity.float 0.2
    static member reciprocal(quantity: Quantity<'Units>) : Quantity<'Units> = Quantity(1. / quantity.Value)


    /// Returns the remainder of the modulus operation.
    /// Note: This returns negative results for remainders on negative numbers.
    static member modBy (modulus: Quantity<'Units>) (quantity: Quantity<'Units>) : Quantity<'Units> = quantity % modulus


    /// Returns the remainder of the modulus operation.
    /// Note: This returns positive results for remainders on negative numbers.
    static member remainderBy (modulus: Quantity<'Units>) (quantity: Quantity<'Units>) : Quantity<'Units> =
        abs (quantity % modulus)


    /// Interpolate from the first quantity to the second, based on a parameter that
    /// ranges from zero to one. Passing a parameter quantity of zero will return the start
    /// quantity and passing a parameter quantity of one will return the end quantity.
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
    /// The end quantity can be less than the start quantity:
    ///     Quantity.interpolateFrom tenMeters fiveMeters 0.1
    ///     --> Length.meters 9.5
    /// Parameter quantitys less than zero or greater than one can be used to extrapolate:
    ///     Quantity.interpolateFrom fiveMeters tenMeters 1.5
    ///     --> Length.meters 12.5
    ///     Quantity.interpolateFrom fiveMeters tenMeters -0.5
    ///     --> Length.meters 2.5
    ///     Quantity.interpolateFrom tenMeters fiveMeters -0.2
    ///     --> Length.meters 11
    static member interpolateFrom
        (start: Quantity<'Units>)
        (finish: Quantity<'Units>)
        (parameter: float)
        : Quantity<'Units> =
        if parameter <= 0.5 then
            Quantity(
                start.Value
                + parameter * (finish.Value - start.Value)
            )

        else
            Quantity(
                finish.Value
                + (1. - parameter) * (start.Value - finish.Value)
            )


    /// Find the midpoint between two quantities.
    ///    Quantity.midpoint (Length.meters 5) (Length.meters 10)
    ///    --> Length.meters 7.5
    static member midpoint (x: Quantity<'Units>) (y: Quantity<'Units>) : Quantity<'Units> = x + 0.5 * (y - x)


    /// Construct a range of evenly-spaced quantitys given a <c>start</c> quantity, an <c>end</c>
    /// quantity and the number of <c>steps</c> to take from the start to the end. The first
    /// quantity in the returned list will be equal to <c>start</c> and the last quantity will be
    /// equal to <c>end</c>. Note that the number of returned quantitys will be one greater than
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
    /// The start and end quantitys can be in either order:
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
    /// Passing a negative or zero <c>steps</c> quantity will result in an empty list being
    /// returned.
    /// If you need finer control over what quantitys get generated, try combining
    /// <c>interpolateFrom</c> with the various functions in the
    /// [<c>elm-1d-parameter</c>](https://package.elm-lang.org/packages/ianmackenzie/elm-1d-parameter/latest/)
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
    ///     -- Omit the last quantity
    ///     Parameter1d.leading 4 <|
    ///         Quantity.interpolateFrom
    ///             (Length.meters 2)
    ///             (Length.meters 3)
    ///     --> [ Length.centimeters 200
    ///     --> , Length.centimeters 225
    ///     --> , Length.centimeters 250
    ///     --> , Length.centimeters 275
    ///     --> ]
    static member range (start: Quantity<'Units>) (finish: Quantity<'Units>) (steps: int) : Quantity<'Units> list =
        let rec rangeHelp
            start
            finish
            (i: int)
            (steps: float)
            (accumulatedValues: Quantity<'Units> list)
            : Quantity<'Units> list =
            let quantity =
                Quantity.interpolateFrom start finish (float i / steps)

            let updatedValues =
                quantity :: accumulatedValues

            if i = 0 then
                updatedValues

            else
                rangeHelp start finish (i - 1) steps updatedValues

        if steps > 0 then
            rangeHelp start finish steps (float steps) []

        else
            []


    /// Generalized units conversion function that lets you convert to many kinds of
    /// units not directly supported by <c>elm-units</c>. The first argument is a function
    /// that constructs a quantity of the desired unit type, and the second is the quantity
    /// to convert. For example,
    ///     Speed.metersPerSecond 5
    ///         |> Speed.inFeetPerSecond
    ///     --> 16.4042
    /// is equivalent to
    ///     Speed.metersPerSecond 5
    ///         |> Quantity.in_ Speed.feetPerSecond
    ///     --> 16.4042
    /// More interestingly, if you wanted to get speed in some weirder unit like
    /// millimeters per minute (not directly supported by <c>elm-units</c>), you could do
    ///     Speed.metersPerSecond 5
    ///         |> Quantity.in_
    ///             (Length.millimeters
    ///                 >> Quantity.per (Duration.minutes 1)
    ///             )
    ///     --> 300000
    /// Internally,
    ///     Quantity.in_ someUnits someQuantity
    /// is simply implemented as
    ///     Quantity.ratio some(someUnits 1)
    static member in_ (units: float -> 'a) (quantity: Quantity<'Units>) : float = Quantity.ratio quantity (units 1.)


    // ---- Float Conversions ------------------------------------------------------------------------------------------

    static member roundTo (digits: int) (quantity: Quantity<'Units>) : Quantity<'Units> =
        Quantity(Float.roundFloatTo digits quantity.Value)

    /// Round a <c>Float</c>-valued quantity to the nearest <c>Int</c>. Note that [this may
    /// not do what you expect](#-int-float-conversion).
    ///     Quantity.round (Pixels.pixels 3.5)
    ///     --> Pixels.pixels 4
    static member round(quantity: Quantity<'Units>) : Quantity<'Units> = Quantity(round quantity.Value)



    /// Round a <c>Float</c>-valued quantity down to the nearest <c>Int</c>. Note that [this
    /// may not do what you expect](#-int-float-conversion).
    ///     Quantity.floor (Pixels.pixels 2.9)
    ///     --> Pixels.pixels 2
    ///     Quantity.floor (Pixels.pixels -2.1)
    ///     --> Pixels.pixels -3
    static member floor(quantity: Quantity<'Units>) : Quantity<'Units> = floor quantity


    /// Round a <c>Float</c>-valued quantity up to the nearest <c>Int</c>. Note that [this may
    /// not do what you expect](#-int-float-conversion).
    ///     Quantity.ceiling (Pixels.pixels 1.2)
    ///     --> Pixels.pixels 2
    ///     Quantity.ceiling (Pixels.pixels -2.1)
    ///     --> Pixels.pixels -2
    static member ceil(quantity: Quantity<'Units>) : Quantity<'Units> = ceil quantity


    /// Round a <c>Float</c>-valued quantity towards zero. Note that [this may not do
    /// what you expect](#-int-float-conversion).
    ///     Quantity.truncate (Pixels.pixels -2.8)
    ///     --> Pixels.pixels -2
    static member truncate(quantity: Quantity<'Units>) : Quantity<'Units> = truncate quantity

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
    static member sum(quantities: Quantity<'Units> list) : Quantity<'Units> =
        List.fold Quantity.plus Quantity.zero quantities


    /// Find the minimum quantity in a list of quantities. Returns <c>Nothing</c> if the
    /// list is empty.
    ///    Quantity.minimum
    ///        [ Mass.kilograms 1
    ///        , Mass.pounds 2
    ///        , Mass.tonnes 3
    ///        ]
    ///    --> Just (Mass.pounds 2)
    static member minimum(quantities: Quantity<'Units> list) : Quantity<'Units> option =
        match quantities with
        | [] -> None

        | first :: rest -> Some(List.fold min first rest)


    /// Find the maximum quantity in a list of quantities. Returns <c>Nothing</c> if the
    /// list is empty.
    ///     Quantity.maximum
    ///         [ Mass.kilograms 1
    ///         , Mass.pounds 2
    ///         , Mass.tonnes 3
    ///         ]
    ///     --> Just (Mass.tonnes 3)
    static member maximum(quantities: Quantity<'Units> list) : Quantity<'Units> option =
        match quantities with
        | [] -> None

        | first :: rest -> Some(List.fold max first rest)


    /// Find the 'minimum' item in a list as measured by some derived <c>Quantity</c>:
    ///     people =
    ///         [ { name = "Bob", height = Length.meters 1.6 }
    ///         , { name = "Charlie", height = Length.meters 2.0 }
    ///         , { name = "Alice", height = Length.meters 1.8 }
    ///         ]
    ///     Quantity.minimumBy .height people
    ///     --> Just { name = "Bob", height = Length.meters 1.6 }
    /// If the list is empty, returns <c>Nothing</c>. If multiple items in the list are tied,
    /// then the first one is returned.
    static member minimumBy (toQuantity: 'a -> Quantity<'Units>) (quantities: 'a list) : 'a option =
        let rec minimumByHelp currentItem currentValue (currentQuantities: 'a list) =
            match currentQuantities with
            | [] -> currentItem

            | item :: rest ->
                let quantity = toQuantity item

                if quantity < currentValue then
                    minimumByHelp item quantity rest

                else
                    minimumByHelp currentItem currentValue rest

        match quantities with
        | [] -> None

        | firstItem :: rest ->
            let firstValue = toQuantity firstItem
            Some(minimumByHelp firstItem firstValue rest)




    /// Find the 'maximum' item in a list as measured by some derived <c>Quantity</c>:
    ///     people =
    ///         [ { name = "Bob", height = Length.meters 1.6 }
    ///         , { name = "Charlie", height = Length.meters 2.0 }
    ///         , { name = "Alice", height = Length.meters 1.8 }
    ///         ]
    ///     Quantity.maximumBy .height people
    ///     --> Just { name = "Charlie", height = Length.meters 2.0 }
    /// If the list is empty, returns <c>Nothing</c>. If multiple items in the list are tied,
    /// then the first one is returned.
    static member maximumBy (toQuantity: 'a -> Quantity<'Units>) (quantities: 'a list) : 'a option =
        let rec maximumByHelp currentItem currentValue (currentQuantities: 'a list) =
            match currentQuantities with
            | [] -> currentItem

            | item :: rest ->
                let quantity = toQuantity item

                if quantity < currentValue then
                    maximumByHelp item quantity rest

                else
                    maximumByHelp currentItem currentValue rest

        match quantities with
        | [] -> None

        | firstItem :: rest ->
            let firstValue = toQuantity firstItem
            Some(maximumByHelp firstItem firstValue rest)



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
    static member sort(quantities: Quantity<'Units> list) : Quantity<'Units> list =
        List.sortBy Quantity.unwrap quantities


    /// Sort an arbitrary list of quantitys by a derived <c>Quantity</c>. If you had
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
    static member sortBy (toQuantity: 'a -> Quantity<'Units>) (list: 'a list) : 'a list =
        let comparator first second =
            compare (toQuantity first) (toQuantity second)

        List.sortWith comparator list



    // ---- Working With Rates -----------------------------------------------------


    /// Construct a rate of change by dividing a dependent quantity (numerator) by
    /// an independent quantity (denominator):
    ///     speed =
    ///         Quantity.rate (Length.miles 1) Duration.minute
    ///     speed |> Speed.inMilesPerHour
    ///     --> 60
    /// Note that we could directly use our rate of change quantity as a <c>Speed</c>! That is
    /// because many built-in quantity types are defined as rates of change, for
    /// example:
    ///   - <c>Speed</c> is <c>Length</c> per <c>Duration</c>
    ///   - <c>Acceleration</c> is <c>Speed</c> per <c>Duration</c>
    ///   - <c>Pressure</c> is <c>Force</c> per <c>Area</c>
    ///   - <c>Power</c> is <c>Energy</c> per <c>Duration</c>
    ///   - <c>Current</c> is <c>Charge</c> per <c>Duration</c>
    ///   - <c>Resistance</c> is <c>Voltage</c> per <c>Current</c>
    ///   - <c>Voltage</c> is <c>Power</c> per <c>Current</c>
    /// Note that there are [other forms of division](/#multiplication-and-division)!
    static member rate
        (dependentValue: Quantity<'Dependent>)
        (independentValue: Quantity<'Independent>)
        : Quantity<Rate<'Dependent, 'Independent>> =
        Quantity(dependentValue.Value / independentValue.Value)

    /// 'Infix' version of [<c>rate</c>](#rate), meant to be used in pipeline form;
    ///     Quantity.rate distance time
    /// can be written as
    ///     distance |> Quantity.per time
    static member per
        (independentValue: Quantity<'Independent>)
        (dependentValue: Quantity<'Dependent>)
        : Quantity<Rate<'Dependent, 'Independent>> =
        Quantity(dependentValue.Value / independentValue.Value)


    /// Multiply a rate of change by an independent quantity (the denominator in
    /// the rate) to get a total quantity:
    ///
    ///       Duration.minutes 30
    ///           |> Quantity.at
    ///               (Speed.kilometersPerHour 100)
    ///       --> Length.kilometers 50
    ///
    /// Can be useful to define conversion functions from one unit to another, since
    /// if you define a <c>rate</c> then <c>Quantity.at rate</c> will give you a conversion
    /// function:
    ///
    ///<c></c>`fsharp
    ///       pixelDensity : Float (Rate Pixels Meters)
    ///       pixelDensity =
    ///           Pixels.pixels 96 |> Quantity.per (Length.inches 1)
    ///       lengthToPixels : Length -> Float Pixels
    ///       lengthToPixels length =
    ///           Quantity.at pixelDensity length
    ///       lengthToPixels (Length.inches 3)
    ///       --> Pixels.pixels 288
    ///<c></c>`
    ///
    /// Eagle-eyed readers will note that using partial application you could also
    /// simply write
    ///
    ///     lengthToPixels =
    ///         Quantity.at pixelDensity
    ///
    /// Note that there are [other forms of multiplication](/#multiplication-and-division)!
    static member at
        (rateOfChange: Quantity<Rate<'Dependent, 'Independent>>)
        (independentValue: Quantity<'Independent>)
        : Quantity<'Dependent> =
        Quantity(rateOfChange.Value * independentValue.Value)


    /// Given a rate and a _dependent_ quantity (total quantity), determine the
    /// necessary amount of the _independent_ quantity:
    ///     Length.kilometers 75
    ///         |> Quantity.at_
    ///             (Speed.kilometersPerHour 100)
    ///     --> Duration.minutes 45
    /// Where <c>at</c> performs multiplication, <c>at_</c> performs division - you multiply a
    /// speed by a duration to get a distance, but you divide a distance by a speed to
    /// get a duration.
    /// Similar to <c>at</c>, <c>at_</c> can be used to define an _inverse_ conversion function:
    ///     pixelDensity : Float (Rate Pixels Meters)
    ///     pixelDensity =
    ///         Pixels.pixels 96 |> Quantity.per (Length.inches 1)
    ///     pixelsToLength : Float Pixels -> Length
    ///     pixelsToLength pixels =
    ///         Quantity.at_ pixelDensity pixels
    ///     pixelsToLength (Pixels.pixels 48)
    ///     --> Length.inches 0.5
    ///     Float (Rate dependentUnits independentUnits)
    //    -> Float dependentUnits
    //    -> Float independentUnits
    static member at_
        (rateOfChange: Quantity<Rate<'Dependent, 'Independent>>)
        (dependentValue: Quantity<'Depenent>)
        : Quantity<'Independent> =
        Quantity(dependentValue.Value / rateOfChange.Value)


    /// Same as <c>at</c> but with the argument order flipped, which may read better
    /// in some cases:
    ///     Speed.kilometersPerHour 100
    ///         |> Quantity.for
    ///             (Duration.minutes 30)
    ///     --> Length.kilometers 50
    static member for_
        (independentValue: Quantity<'Independent>)
        (rateOfChange: Quantity<Rate<'Dependent, 'Independent>>)
        : Quantity<'Dependent> =
        Quantity(rateOfChange.Value * independentValue.Value)


    /// Find the inverse of a given rate. May be useful if you are using a rate to
    /// define a conversion, and want to convert the other way;
    ///     Quantity.at (Quantity.inverse rate)
    /// is equivalent to
    ///     Quantity.at_ rate
    static member inverse
        (rateOfChange: Quantity<Rate<'Dependent, 'Independent>>)
        : Quantity<Rate<'Independent, 'Dependent>> =
        Quantity(1. / rateOfChange.Value)


    /// Multiply two rates of change that 'cancel out' together, resulting in a new
    /// rate. For example, if you know the real-world speed of an on-screen object and
    /// the display resolution, then you can get the speed in pixels per second:
    ///     realWorldSpeed =
    ///         Speed.metersPerSecond 0.1
    ///     resolution =
    ///         Pixels.float 96 |> Quantity.per Length.inch
    ///     Quantity.rateProduct realWorldSpeed resolution
    ///     --> Pixels.pixelsPerSecond 377.95
    /// That is, "length per duration" multiplied by "pixels per length" gives you
    /// "pixels per duration".
    /// Sometimes you can't directly multiply two rates to get what you want, in which
    /// case you may need to use [<c>inverse</c>](#inverse) in combination with
    /// <c>rateProduct</c>. For example, if you know the on-screen speed of some object and
    /// the display resolution, then you can use those to get the real-world speed:
    ///     pixelSpeed =
    ///         Pixels.pixelsPerSecond 500
    ///     resolution =
    ///         Pixels.float 96 |> Quantity.per Length.inch
    ///     Quantity.rateProduct pixelSpeed
    ///         (Quantity.inverse resolution).Value
    ///     --> Speed.metersPerSecond 0.1323
    static member rateProduct
        (firstRate: Quantity<Rate<'U2, 'U1>>)
        (secondRate: Quantity<Rate<'U3, 'U2>>)
        : Quantity<Rate<'U3, 'U1>> =
        Quantity(firstRate.Value * secondRate.Value)
