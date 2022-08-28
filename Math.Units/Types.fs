﻿namespace Math.Units

open System
open Math.Units

// ---- Unit Systems -----------------------------------------------------------

/// A special units type representing 'no units'. A `Quantity Int Unitless`
/// value is interchangeable with a simple `Int`, and a `Quantity Float Unitless`
/// value is interchangeable with a simple `Float`.
/// A generic number that doesn't undergo any type mutation.
/// Eg.
///     Unitless: Unitless 1. * Unitless 1. = Unitless 1.
///     Meters  : Meters 1. * Meters 1. = (Meters Squared) 1.
type Unitless = Unitless

type Pixel = Pixel
type Meters = Meters
type Kilograms = Kilograms
type Radians = Radians
type Seconds = Seconds
type Coulombs = Coulombs
type Percentage = Percentage
type Lumens = Lumens
type Steradians = Steradians
type Moles = Moles
type CelsiusDegrees = CelsiusDegrees


// ---- Unit Ratios ------------------------------------------------------------



/// Represents a units type that is the product of two other units types. This
/// is a more general form of `Squared` or `Cubed`. See [`product`](#product),
/// [`times`](#times), [`over`](#over) and [`over_`](#over_) for how it can be used.
type Product<'Unit1, 'Unit2> = Product of 'Unit1 * 'Unit2

/// Represents a units type that is the square of some other units type; for
/// example, `Meters` is one units type (the units type of a [`Length`](Length)) and
/// `Squared Meters` is another (the units type of an [`Area`](Area)). See the
/// [`squared`](#squared) and [`sqrt`](#sqrt) functions for examples of use.
/// This is a special case of the `Product` units type.
type Squared<'Units> = Product<'Units, 'Units>

/// Represents a units type that is the cube of some other units type; for
/// example, `Meters` is one units type (the units type of a [`Length`](Length)) and
/// `Cubed Meters` is another (the units type of an [`Volume`](Volume)). See the
/// [`cubed`](Quantity#cubed) and [`cbrt`](Quantity#cbrt) functions for examples of
/// use.
/// This is a special case of the `Product` units type.
type Cubed<'Units> = Product<'Units, Product<'Units, 'Units>>


/// Represents the units type of a rate or quotient such as a speed (`Rate
/// Meters Seconds`) or a pressure (`Rate Newtons SquareMeters`). See [Working with
/// rates](#working-with-rates) for details.
type Rate<'DependentUnits, 'IndependentUnits> = Rate of 'DependentUnits * 'IndependentUnits


// ---- Unit Aliases -----------------------------------------------------------

// ---- Angular
type RadiansPerSecond = Rate<Meters, Seconds>
type RadiansPerSecondSquared = Rate<RadiansPerSecond, Seconds>


// ---- Distance
type MetersPerSecond = Rate<Meters, Seconds>
type MetersPerSecondSquared = Rate<MetersPerSecond, Seconds>
type SquareMeters = Squared<Meters>
type CubicMeters = Cubed<Meters>


// ---- Mass
type Newtons = Product<Kilograms, MetersPerSecondSquared>
type Pascals = Rate<Newtons, SquareMeters>
type KilogramsPerCubicMeter = Rate<Kilograms, CubicMeters>
type Joules = Product<Newtons, Meters>


// ---- Light
type Candelas = Rate<Lumens, Steradians>
type Lux = Rate<Lumens, SquareMeters>
type Nits = Rate<Candelas, SquareMeters>


// ---- Atomic
type MolesPerCubicMeter = Rate<Moles, CubicMeters>


// ---- Electrical
type Watts = Rate<Joules, Seconds>
type Amperes = Rate<Coulombs, Seconds>
type Volts = Rate<Watts, Amperes>
type Farads = Rate<Coulombs, Volts>
type Henries = Rate<Volts, Rate<Amperes, Seconds>>
type Ohms = Rate<Volts, Amperes>


// ---- Pixels
type PixelsPerSecond = Rate<Pixel, Seconds>
type PixelsPerSecondSquared = Rate<PixelsPerSecond, Seconds>
type SquarePixels = Squared<Pixel>


// ---- Quantity Declaration ---------------------------------------------------

/// A `Quantity` is effectively a `number` (an `Int` or `Float`) tagged with a
/// `units` type. So a
///     Quantity Float Meters
/// is a `Float` number of `Meters` and a
///     Quantity Int Pixels
/// is an `Int` number of `Pixels`. When compiling with `elm make --optimize` the
/// `Quantity` wrapper type will be compiled away, so the runtime performance should
/// be comparable to using a raw `Float` or `Int`.
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
type Percent = Quantity<Percentage>

type Duration = Quantity<Seconds>


// ---- Distance
type Length = Quantity<Meters>
type Area = Quantity<SquareMeters>
type Volume = Quantity<CubicMeters>
type Pixels = Quantity<Pixel>
type Speed = Quantity<MetersPerSecond>
type Acceleration = Quantity<MetersPerSecondSquared>


// ---- Angular
type Angle = Quantity<Radians>
type AngularSpeed = Quantity<RadiansPerSecond>
type AngularAcceleration = Quantity<RadiansPerSecondSquared>
type SolidAngle = Quantity<Steradians>


// ---- Mass
type Mass = Quantity<Kilograms>
type Density = Quantity<KilogramsPerCubicMeter>
type Force = Quantity<Newtons>
type Energy = Quantity<Joules>
type Pressure = Quantity<Pascals>

// ---- Light
type LuminousFlux = Quantity<Lumens>
type LuminousIntensity = Quantity<Candelas>
type Illuminance = Quantity<Lux>
type Luminance = Quantity<Nits>

// ---- Atomic
type SubstanceAmount = Quantity<Moles>
type Molarity = Quantity<MolesPerCubicMeter>

// ---- Electrical
type Charge = Quantity<Coulombs>
type Current = Quantity<Amperes>
type Capacitance = Quantity<Farads>
type Inductance = Quantity<Henries>
type Power = Quantity<Watts>
type Resistance = Quantity<Ohms>
type Voltage = Quantity<Volts>

// ---- Temperature ------------------------------------------------------------


type Delta = Quantity<CelsiusDegrees>

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
