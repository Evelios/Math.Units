﻿namespace Math.Units

open System
open Math.Units

// ---- Unit Systems -----------------------------------------------------------

/// <category>Unit Space</category>
/// <summary>
/// A special units type representing 'no units'. A <c>Quantity Unitless</c>
/// value is interchangeable with a simple <c>float</c>.
/// A generic number that doesn't undergo any type mutation.
/// Eg.
/// <code lang="fsharp">
///     Unitless: Unitless 1. * Unitless 1. = Unitless 1.
///     Meters  : Meters 1. * Meters 1. = (Meters Squared) 1.
/// </code>
/// </summary>
type Unitless = Unitless

/// <category>Unit Space</category>
type Pixel = Pixel
/// <category>Unit Space</category>
type Meters = Meters
/// <category>Unit Space</category>
type Kilograms = Kilograms
/// <category>Unit Space</category>
type Radians = Radians
/// <category>Unit Space</category>
type Seconds = Seconds
/// <category>Unit Space</category>
type Coulombs = Coulombs
/// <category>Unit Space</category>
type Percentage = Percentage
/// <category>Unit Space</category>
type Lumens = Lumens
/// <category>Unit Space</category>
type Steradians = Steradians
/// <category>Unit Space</category>
type Moles = Moles
/// <category>Unit Space</category>
type CelsiusDegrees = CelsiusDegrees


// ---- Unit Ratios ------------------------------------------------------------



/// <category>Unit Relation</category>
/// <summary>
/// Represents a units type that is the product of two other units types. This
/// is a more general form of <c>Squared</c> or <c>Cubed</c>. See
/// <see cref="M:Math.Units.Quantity.product">Quantity.product</see>,
/// <see cref="M:Math.Units.Quantity.times">Quantity.times</see>,
/// <see cref="M:Math.Units.Quantity.over">Quantity.over</see> and
/// <see cref="M:Math.Units.Quantity.over_">Quantity.over_</see> for how it can be used.
/// </summary>
type Product<'Unit1, 'Unit2> = Product of 'Unit1 * 'Unit2

/// <category>Unit Relation</category>
/// <summary>
/// Represents a units type that is the square of some other units type; for
/// example, <c>Meters</c> is one units type (the units type of a <c>Length</c>) and
/// <c>Squared Meters</c> is another (the units type of an <c>Area</c>). See the
/// <see cref="Math.Units.Quantity.squared">Quantity.squared</see> and [<c>sqrt</c>](#sqrt)
/// <see cref="Math.Units.Quantity.sqrt">Quantity.sqrt</see>
/// functions for examples of use. This is a special case of the <c>Product</c> units type.
/// </summary>
type Squared<'Units> = Product<'Units, 'Units>

/// <category>Unit Relation</category>
/// <summary>
/// Represents a units type that is the cube of some other units type; for
/// example, <c>Meters</c> is one units type (the units type of a [<c>Length</c>](Length)) and
/// <c>Cubed Meters</c> is another (the units type of an [<c>Volume</c>](Volume)). See the
/// [<c>cubed</c>](Quantity#cubed) and [<c>cbrt</c>](Quantity#cbrt) functions for examples of
/// use.
/// This is a special case of the <c>Product</c> units type.
/// </summary>
type Cubed<'Units> = Product<'Units, Product<'Units, 'Units>>


/// <category>Unit Relation</category>
/// <summary>
/// Represents the units type of a rate or quotient such as a speed (`Rate
/// Meters Seconds<c>) or a pressure (</c>Rate Newtons SquareMeters`). See [Working with
/// rates](#working-with-rates) for details.
/// </summary>
type Rate<'DependentUnits, 'IndependentUnits> = Rate of 'DependentUnits * 'IndependentUnits


// ---- Unit Aliases -----------------------------------------------------------

// ---- Angular
/// <category>Unit Space</category>
type RadiansPerSecond = Rate<Radians, Seconds>
/// <category>Unit Space</category>
type RadiansPerSecondSquared = Rate<RadiansPerSecond, Seconds>


// ---- Distance

/// <category>Unit Space</category>
type MetersPerSecond = Rate<Meters, Seconds>
/// <category>Unit Space</category>
type MetersPerSecondSquared = Rate<MetersPerSecond, Seconds>
/// <category>Unit Space</category>
type SquareMeters = Squared<Meters>
/// <category>Unit Space</category>
type CubicMeters = Cubed<Meters>


// ---- Mass

/// <category>Unit Space</category>
type Newtons = Product<Kilograms, MetersPerSecondSquared>
/// <category>Unit Space</category>
type Pascals = Rate<Newtons, SquareMeters>
/// <category>Unit Space</category>
type KilogramsPerCubicMeter = Rate<Kilograms, CubicMeters>
/// <category>Unit Space</category>
type Joules = Product<Newtons, Meters>


// ---- Light

/// <category>Unit Space</category>
type Candelas = Rate<Lumens, Steradians>
/// <category>Unit Space</category>
type Lux = Rate<Lumens, SquareMeters>
/// <category>Unit Space</category>
type Nits = Rate<Candelas, SquareMeters>


// ---- Atomic

/// <category>Unit Space</category>
type MolesPerCubicMeter = Rate<Moles, CubicMeters>


// ---- Electrical

/// <category>Unit Space</category>
type Watts = Rate<Joules, Seconds>
/// <category>Unit Space</category>
type Amperes = Rate<Coulombs, Seconds>
/// <category>Unit Space</category>
type Volts = Rate<Watts, Amperes>
/// <category>Unit Space</category>
type Farads = Rate<Coulombs, Volts>
/// <category>Unit Space</category>
type Henries = Rate<Volts, Rate<Amperes, Seconds>>
/// <category>Unit Space</category>
type Ohms = Rate<Volts, Amperes>


// ---- Pixels


/// <category>Unit Space</category>
type PixelsPerSecond = Rate<Pixel, Seconds>
/// <category>Unit Space</category>
type PixelsPerSecondSquared = Rate<PixelsPerSecond, Seconds>
/// <category>Unit Space</category>
type SquarePixels = Squared<Pixel>


// ---- Quantity Declaration ---------------------------------------------------

/// <category>Unit Type</category>
/// <summary>
/// A <c>Quantity</c> is effectively a <c>number</c> (an <c>Int</c> or <c>Float</c>) tagged with a
/// <c>units</c> type. So a
///     Quantity Float Meters
/// is a <c>Float</c> number of <c>Meters</c> and a
///     Quantity Int Pixels
/// is an <c>Int</c> number of <c>Pixels</c>. When compiling with <c>elm make --optimize</c> the
/// <c>Quantity</c> wrapper type will be compiled away, so the runtime performance should
/// be comparable to using a raw <c>Float</c> or <c>Int</c>.
/// </summary>
type Quantity<'Units>(quantity: float) =
    interface IComparable<Quantity<'Units>> with
        member this.CompareTo(quantity: Quantity<'Units>) : int = this.Comparison(quantity)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Quantity<'Units> as quantity -> this.Comparison(quantity)
            | _ -> -1


    // ---- Base Properties & Functions ----

    member this.Value = quantity

    override this.ToString() = $"{quantity} {typeof<'Units>.Name}"


    // ---- IComparable Implementation ----

    member this.Comparison(other: Quantity<'Units>) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1


    member this.Equals(other: Quantity<'Units>) : bool =
        Float.almostEqual this.Value other.Value


    member this.LessThan(other: Quantity<'Units>) = this.Value < other.Value


    override this.GetHashCode() =
        this.Value
        |> Float.roundFloatTo Float.DigitPrecision
        |> hash


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Quantity<'Units> as other -> this.Equals(other)
        | _ -> false


    // ---- Built In Functions ----

    static member Abs(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(abs q.Value)

    static member Min(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(min lhs.Value rhs.Value)

    static member Max(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(max lhs.Value rhs.Value)

    static member Sqrt(value: Quantity<'Units Squared>) : Quantity<'Units> = Quantity<'Units>(sqrt value.Value)

    static member Floor(value: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(floor value.Value)

    static member Ceiling(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(ceil q.Value)

    static member Round(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(round q.Value)

    static member Truncate(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(truncate q.Value)


    // ---- Operators ----

    static member (+)(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(lhs.Value + rhs.Value)

    static member (-)(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(lhs.Value - rhs.Value)

    static member (~-)(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(-q.Value)

    static member (*)(q: Quantity<'Units>, scale: float) : Quantity<'Units> = Quantity<'Units>(q.Value * scale)

    static member (*)(scale: float, q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(q.Value * scale)

    static member (*)(lhs: Quantity<'UnitA>, rhs: Quantity<'UnitB>) : Quantity<Product<'UnitA, 'UnitB>> =
        Quantity<Product<'UnitA, 'UnitB>>(lhs.Value * rhs.Value)

    static member (/)(q: Quantity<'Units>, scale: float) : Quantity<'Units> = Quantity<'Units>(q.Value / scale)

    static member (/)(q: Quantity<'Units>, scale: Quantity<'Units>) : float = q.Value / scale.Value

    static member (/)
        (
            dependent: Quantity<'Dependent>,
            independent: Quantity<'Independent>
        ) : Quantity<Rate<'Dependent, 'Independent>> =
        Quantity(dependent.Value / independent.Value)

    static member (%)(q: Quantity<'Units>, modulus: Quantity<'Units>) : Quantity<'Units> =
        Quantity(q.Value % modulus.Value)


// ---- Quantity Types ---------------------------------------------------------


/// A percentage value. The default range for percentages is 0 to 1 but can also be given in the range 0 to 100.
///
/// <category>Unit Type</category>
type Percent = Quantity<Percentage>

/// <category>Unit Type</category>
type Duration = Quantity<Seconds>


// ---- Distance

/// <category>Unit Type</category>
type Length = Quantity<Meters>
/// <category>Unit Type</category>
type Area = Quantity<SquareMeters>
/// <category>Unit Type</category>
type Volume = Quantity<CubicMeters>
/// <category>Unit Type</category>
type Pixels = Quantity<Pixel>
/// <category>Unit Type</category>
type Speed = Quantity<MetersPerSecond>
/// <category>Unit Type</category>
type Acceleration = Quantity<MetersPerSecondSquared>


// ---- Angular


/// <category>Unit Type</category>
type Angle = Quantity<Radians>
/// <category>Unit Type</category>
type AngularSpeed = Quantity<RadiansPerSecond>
/// <category>Unit Type</category>
type AngularAcceleration = Quantity<RadiansPerSecondSquared>
/// <category>Unit Type</category>
type SolidAngle = Quantity<Steradians>


// ---- Mass


/// <category>Unit Type</category>
type Mass = Quantity<Kilograms>
/// <category>Unit Type</category>
type Density = Quantity<KilogramsPerCubicMeter>
/// <category>Unit Type</category>
type Force = Quantity<Newtons>
/// <category>Unit Type</category>
type Energy = Quantity<Joules>
/// <category>Unit Type</category>
type Pressure = Quantity<Pascals>

// ---- Light


/// <category>Unit Type</category>
type LuminousFlux = Quantity<Lumens>
/// <category>Unit Type</category>
type LuminousIntensity = Quantity<Candelas>
/// <category>Unit Type</category>
type Illuminance = Quantity<Lux>
/// <category>Unit Type</category>
type Luminance = Quantity<Nits>

// ---- Atomic


/// <category>Unit Type</category>
type SubstanceAmount = Quantity<Moles>
/// <category>Unit Type</category>
type Molarity = Quantity<MolesPerCubicMeter>

// ---- Electrical


/// <category>Unit Type</category>
type Charge = Quantity<Coulombs>
/// <category>Unit Type</category>
type Current = Quantity<Amperes>
/// <category>Unit Type</category>
type Capacitance = Quantity<Farads>
/// <category>Unit Type</category>
type Inductance = Quantity<Henries>
/// <category>Unit Type</category>
type Power = Quantity<Watts>
/// <category>Unit Type</category>
type Resistance = Quantity<Ohms>
/// <category>Unit Type</category>
type Voltage = Quantity<Volts>

// ---- Temperature ------------------------------------------------------------


/// <category>Unit Type</category>
type Delta = Quantity<CelsiusDegrees>

/// <category>Unit Type</category>
type Temperature(kelvin: float) =
    interface IComparable<Temperature> with
        member this.CompareTo(temp: Temperature) : int = this.Comparison(temp)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Temperature as temp -> this.Comparison(temp)
            | _ -> -1


    // ---- Base Properties & Functions ----

    member this.Value = kelvin

    override this.ToString() = $"{kelvin}K"


    // ---- IComparable Implementation ----

    member this.Comparison(other: Temperature) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1


    member this.Equals(other: Temperature) : bool =
        Float.almostEqual this.Value other.Value


    member this.LessThan(other: Temperature) = this.Value < other.Value


    override this.GetHashCode() =
        this.Value
        |> Float.roundFloatTo Float.DigitPrecision
        |> hash


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Temperature as other -> this.Equals(other)
        | _ -> false


    // ---- Built In Functions ----

    static member Abs(q: Temperature) : Temperature = Temperature(abs q.Value)

    static member Min(lhs: Temperature, rhs: Temperature) : Temperature = Temperature(min lhs.Value rhs.Value)

    static member Max(lhs: Temperature, rhs: Temperature) : Temperature = Temperature(max lhs.Value rhs.Value)

    static member Sqrt(value: Quantity<'Units Squared>) : Temperature = Temperature(sqrt value.Value)

    static member Floor(value: Temperature) : Temperature = Temperature(floor value.Value)

    static member Ceiling(q: Temperature) : Temperature = Temperature(ceil q.Value)

    static member Round(q: Temperature) : Temperature = Temperature(round q.Value)

    static member Truncate(q: Temperature) : Temperature = Temperature(truncate q.Value)


    // ---- Operators ----

    static member (+)(lhs: Temperature, rhs: Delta) : Temperature = Temperature(lhs.Value + rhs.Value)

    static member (+)(lhs: Delta, rhs: Temperature) : Temperature = Temperature(lhs.Value + rhs.Value)

    static member (-)(lhs: Temperature, rhs: Temperature) : Delta = Delta(lhs.Value - rhs.Value)


// ---- Interval ---------------------------------------------------------------


/// A finite, closed interval with a minimum and maximum number. This can
/// represent an interval of any type.
///
/// For example...
///     Interval float
///     Interval int
///     Interval Angle
///
/// <category index=4>Advanced Types</category>
[<CustomEquality; NoComparison>]
type Interval<'Units> =
    | Interval of Start: Quantity<'Units> * Finish: Quantity<'Units>

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Interval<'Units> as other ->
            match this, other with
            | Interval (thisStart, thisFinish), Interval (otherStart, otherFinish) ->
                thisStart = otherStart && thisFinish = otherFinish

        | _ -> false

    override this.ToString() =
        match this with
        | Interval (start, finish) -> $"Interval [ {start} -> {finish} ]"

    override this.GetHashCode() : int =
        match this with
        | Interval (start, finish) -> HashCode.Combine(start, finish)
