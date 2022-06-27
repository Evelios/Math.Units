namespace Units

open System

/// Represents a units type that is the square of some other units type; for
/// example, `Meters` is one units type (the units type of a [`Length`](Length)) and
/// `Squared Meters` is another (the units type of an [`Area`](Area)). See the
/// [`squared`](#squared) and [`sqrt`](#sqrt) functions for examples of use.
/// This is a special case of the `Product` units type.
type Squared<'Units> = Product of 'Units * 'Units


/// Represents a units type that is the product of two other units types. This
/// is a more general form of `Squared` or `Cubed`. See [`product`](#product),
/// [`times`](#times), [`over`](#over) and [`over_`](#over_) for how it can be used.
type Product<'Unit1, 'Unit2> = Product of 'Unit1 * 'Unit2


/// Represents a units type that is the cube of some other units type; for
/// example, `Meters` is one units type (the units type of a [`Length`](Length)) and
/// `Cubed Meters` is another (the units type of an [`Volume`](Volume)). See the
/// [`cubed`](Quantity#cubed) and [`cbrt`](Quantity#cbrt) functions for examples of
/// use.
/// This is a special case of the `Product` units type.
type Cubed<'Units> = Cubed of Product<'Units, 'Units> * 'Units


/// Represents the units type of a rate or quotient such as a speed (`Rate
/// Meters Seconds`) or a pressure (`Rate Newtons SquareMeters`). See [Working with
/// rates](#working-with-rates) for details.
type Rate<'DependentUnits, 'IndependentUnits> = Rate of 'DependentUnits * 'IndependentUnits

/// A `Quantity` is effectively a `number` (an `Int` or `Float`) tagged with a
/// `units` type. So a
///     Quantity Float Meters
/// is a `Float` number of `Meters` and a
///     Quantity Int Pixels
/// is an `Int` number of `Pixels`. When compiling with `elm make --optimize` the
/// `Quantity` wrapper type will be compiled away, so the runtime performance should
/// be comparable to using a raw `Float` or `Int`.
[<CustomEquality>]
[<CustomComparison>]
type Quantity<'Units> =
    | Quantity of float

    interface IComparable<Quantity<'Units>> with
        member this.CompareTo(percent) = this.Comparison(percent)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? (Quantity<'Units>) as quantity -> this.Comparison(quantity)
            | _ -> failwith "incompatible comparison"

    member inline this.Equals(Quantity other: Quantity<'Units>) : bool =
        match this with
        | Quantity self -> almostEqual self other

    member inline this.Comparison(other: Quantity<'Units>) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member inline this.LessThan(Quantity other: Quantity<'Units>) =
        match this with
        | Quantity self -> self < other


    // Operators
    static member inline Abs(Quantity value: Quantity<'Units>) : Quantity<'Units> = Quantity(abs value)

    static member inline Min(Quantity lhs: Quantity<'Units>, Quantity rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity(min lhs rhs)

    static member inline Max(Quantity lhs: Quantity<'Units>, Quantity rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity(max lhs rhs)

    static member inline Sqrt(Quantity value: Quantity<'Units Squared>) : Quantity<'Units> = Quantity(sqrt value)

    static member inline Floor(Quantity value: Quantity<'Units>) : Quantity<'Units> = Quantity(floor value)

    static member inline Ceiling(Quantity value: Quantity<'Units>) : Quantity<'Units> = Quantity(ceil value)

    static member inline Round(Quantity value: Quantity<'Units>) : Quantity<'Units> = Quantity(round value)
    static member inline Truncate(Quantity value: Quantity<'Units>) : Quantity<'Units> = Quantity(truncate value)


    static member inline (+)(Quantity lhs: Quantity<'Units>, Quantity rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity(lhs + rhs)

    static member inline (-)(Quantity lhs: Quantity<'Units>, Quantity rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity(lhs - rhs)

    static member inline (~-)(Quantity quantity: Quantity<'Units>) : Quantity<'Units> = Quantity(-quantity)

    static member inline (*)(Quantity quantity: Quantity<'Units>, scale: float) : Quantity<'Units> =
        Quantity(quantity * scale)

    static member inline (*)(scale: float, Quantity quantity: Quantity<'Units>) : Quantity<'Units> =
        Quantity(quantity * scale)

    static member inline (*)
        (
            Quantity lhs: Quantity<'Units>,
            Quantity rhs: Quantity<'Units>
        ) : Quantity<'Units Squared> =
        Quantity(lhs * rhs)

    static member inline (/)(Quantity quantity: Quantity<'Units>, scale: float) : Quantity<'Units> =
        Quantity(quantity / scale)

    static member inline (/)(Quantity quantity: Quantity<'Units>, Quantity scale: Quantity<'Units>) : Quantity<'Units> =
        Quantity(quantity / scale)

    static member inline (%)(Quantity value: Quantity<'Units>, Quantity modulus: Quantity<'Units>) : Quantity<'Units> =
        Quantity(value % modulus)

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Quantity<'Units>) as other -> this.Equals(other)
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Quantity self -> roundFloatTo Float.DigitPrecision self |> hash




// ---- Lengths ----

/// A special units type representing 'no units'. A `Quantity Int Unitless`
/// value is interchangeable with a simple `Int`, and a `Quantity Float Unitless`
/// value is interchangeable with a simple `Float`.
type Unitless = Unitless

type Pixels = Pixels
type Meters = Meters

/// A finite, closed interval with a minimum and maximum number. This can
/// represent an interval of any type.
///
/// For example...
///     Interval float
///     Interval int
///     Interval Angle
[<CustomEquality>]
[<NoComparison>]
type Interval<'T when 'T: equality> =
    | Interval of 'T * 'T

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Interval<'T>) as other ->
            match this, other with
            | Interval (thisStart, thisFinish), Interval (otherStart, otherFinish) ->
                thisStart = otherStart && thisFinish = otherFinish

        | _ -> false

    override this.GetHashCode() : int =
        match this with
        | Interval (start, finish) -> HashCode.Combine(start, finish)



/// A percentage value. The default range for percentages is 0 to 1 but can also be given in the range 0 to 100.
[<CustomEquality>]
[<CustomComparison>]
type Percent =
    | Percent of float

    interface IComparable<Percent> with
        member this.CompareTo(percent) = this.Comparison(percent)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Percent as percent -> this.Comparison(percent)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Percent other: Percent) =
        match this with
        | Percent self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Percent as other -> this.Equals(other)
        | _ -> false

    member this.Equals(Percent other: Percent) : bool =
        match this with
        | Percent self -> almostEqual self other

    override this.GetHashCode() =
        match this with
        | Percent self ->
            (roundFloatTo Float.DigitPrecision self)
                .GetHashCode()


    // Operators

    static member (+)(Percent lhs: Percent, Percent rhs: Percent) : Percent = Percent(lhs + rhs)
    static member (-)(Percent lhs: Percent, Percent rhs: Percent) : Percent = Percent(lhs - rhs)
    static member (*)(Percent percent: Percent, scale: float) : Percent = Percent(percent * scale)
    static member (*)(Percent percent: Percent, Percent scale: Percent) : Percent = Percent(percent * scale)
    static member (/)(Percent percent: Percent, scale: float) : Percent = Percent(percent / scale)
    static member (/)(Percent percent: Percent, Percent scale: Percent) : Percent = Percent(percent / scale)

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Angle =
    | Radians of float

    // Accessors

    /// Convert an arbitrary angle to the equivalent angle in the range -180 to 180
    /// degrees (-π to π radians), by adding or subtracting some multiple of 360
    /// degrees (2π radians) if necessary.
    static member normalize(Radians r) =
        let twoPi = 2. * Math.PI
        let turns = float (int (r / twoPi))
        let radians = r - twoPi * turns

        if (radians > Math.PI) then
            Radians(radians - twoPi)
        else if (radians < -Math.PI) then
            Radians(twoPi + radians)
        else
            Radians radians

    interface IComparable<Angle> with
        member this.CompareTo(angle) = this.Comparison(angle)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Angle as angle -> this.Comparison(angle)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Radians other: Angle) =
        match this with
        | Radians self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Angle as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Angle) : bool =
        match Angle.normalize this, Angle.normalize other with
        | Radians lhs, Radians rhs -> almostEqual lhs rhs

    override this.GetHashCode() =
        match Angle.normalize this with
        | Radians radians ->
            (roundFloatTo Float.DigitPrecision radians)
                .GetHashCode()


    // Math Operators

    static member (~-)(Radians angle: Angle) : Angle = Radians -angle
    static member (+)(Radians lhs: Angle, Radians rhs: Angle) : Angle = Radians(lhs + rhs)
    static member (-)(Radians lhs: Angle, Radians rhs: Angle) : Angle = Radians(lhs - rhs)
    static member (*)(Radians lhs: Angle, rhs: float) : Angle = Radians(lhs * rhs)
    static member (*)(lhs: float, Radians rhs: Angle) : Angle = Radians(rhs * lhs)
    static member (/)(Radians lhs: Angle, rhs: float) : Angle = Radians(lhs / rhs)
    static member (/)(Radians lhs: Angle, Radians rhs: Angle) : float = lhs / rhs


[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Length<'Unit> =
    | Length of float

    // Accessors
    static member create a = Length a

    interface IComparable<Length<'Unit>> with
        member this.CompareTo(length) = this.Comparison(length)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? (Length<'Unit>) as length -> this.Comparison(length)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Length other: Length<'Unit>) =
        match this with
        | Length self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Length<'Unit>) as other -> this.Equals(other)
        | _ -> false

    member this.Equals(Length other: Length<'Unit>) : bool =
        match this with
        | Length self -> almostEqual self other

    override this.GetHashCode() =
        match this with
        | Length self ->
            (roundFloatTo Float.DigitPrecision self)
                .GetHashCode()

    // Unitless Operations
    static member (*)(Length length: Length<'Unit>, Length scale: Length<Unitless>) : Length<'Unit> =
        Length(length * scale)

    static member (*)(Length scale: Length<Unitless>, Length length: Length<'Unit>) : Length<'Unit> =
        Length(scale * length)

    // Generic Operations

    static member (+)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit> = Length(lhs + rhs)
    static member (-)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit> = Length(lhs - rhs)
    static member (~-)(Length length: Length<'Unit>) : Length<'Unit> = Length(-length)
    static member (*)(Length length: Length<'Unit>, scale: float) : Length<'Unit> = Length(length * scale)
    static member (*)(scale: float, Length length: Length<'Unit>) : Length<'Unit> = Length(scale * length)
    static member (*)(Length length: Length<'Unit>, Angle.Radians arc: Angle) : Length<'Unit> = Length(length * arc)
    static member (*)(Angle.Radians arc: Angle, Length length: Length<'Unit>) : Length<'Unit> = Length(arc * length)
    static member (*)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit * 'Unit> = Length(lhs * rhs)
    static member (/)(Length length: Length<'Unit>, scale: float) : Length<'Unit> = Length(length / scale)
    static member (/)(Length length: Length<'Unit>, Length scale: Length<'Unit>) : float = length / scale
    static member (/)(Length length: Length<'Unit>, Angle.Radians arc: Angle) : Length<'Unit> = Length(length / arc)

    // Operator overloads through functions

    static member Pow(Length length: Length<'Unit>, power: float) : Length<'Unit> = Length(length ** power)
