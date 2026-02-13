namespace Math.Units

open System
open Math.Units

// ---- Units -----------------------------------------------------------

/// <category>Unit</category>
/// <summary>
/// A special units type representing 'no units'. A <c>Quantity Unitless</c>
/// value is interchangeable with a simple <c>float</c>.
/// A generic number that doesn't undergo any type mutation.
/// </summary>
///
/// <example>
/// <code lang="fsharp">
///     Unitless: Unitless 1. * Unitless 1. = Unitless 1.
///     Meters  : Meters 1. * Meters 1. = (Meters Squared) 1.
/// </code>
/// </example>
type Unitless = Unitless

/// <category>Unit</category>
type Pixel = Pixel
/// <category>Unit</category>
type Meters = Meters
/// <category>Unit</category>
type Kilograms = Kilograms
/// <category>Unit</category>
type Radians = Radians
/// <category>Unit</category>
type Seconds = Seconds
/// <category>Unit</category>
type Coulombs = Coulombs
/// <category>Unit</category>
type Percentage = Percentage
/// <category>Unit</category>
type Lumens = Lumens
/// <category>Unit</category>
type Steradians = Steradians
/// <category>Unit</category>
type Moles = Moles
/// <category>Unit</category>
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
type Product<'Unit1, 'Unit2> =
    /// <param name="Item1">The first unit.</param>
    /// <param name="Item2">The second unit.</param>
    | Product of 'Unit1 * 'Unit2

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
/// example, <c>Meters</c> is one units type (the units type of a <c>Length</c>) and
/// <c>Cubed Meters</c> is another (the units type of an <c>Volume</c>). See the
/// <c>Quantity.cubed</c> and <c>Quantity.cbrt</c> functions for examples of
/// use.
/// This is a special case of the <c>Product</c> units type.
/// </summary>
type Cubed<'Units> = Product<'Units, Product<'Units, 'Units>>


/// <category>Unit Relation</category>
/// <summary>
/// Represents the units type of a rate or quotient such as a speed (<c>Rate
/// Meters Seconds</c>) or a pressure (<c>Rate Newtons SquareMeters</c>).
/// </summary>
type Rate<'DependentUnits, 'IndependentUnits> =
    /// <param name="Item1">The dependent unit (numerator).</param>
    /// <param name="Item2">The independent unit (denominator).</param>
    | Rate of 'DependentUnits * 'IndependentUnits


// ---- Unit Aliases -----------------------------------------------------------

// ---- Angular
/// <category>Unit</category>
type RadiansPerSecond = Rate<Radians, Seconds>
/// <category>Unit</category>
type RadiansPerSecondSquared = Rate<RadiansPerSecond, Seconds>


// ---- Distance

/// <category>Unit</category>
/// <summary>Meters per second.</summary>
type MetersPerSecond = Rate<Meters, Seconds>
/// <category>Unit</category>
/// <summary>Meters per second squared.</summary>
type MetersPerSecondSquared = Rate<MetersPerSecond, Seconds>
/// <category>Unit</category>
/// <summary>Square meters.</summary>
type SquareMeters = Squared<Meters>
/// <category>Unit</category>
/// <summary>Cubic meters.</summary>
type CubicMeters = Cubed<Meters>


// ---- Mass

/// <category>Unit</category>
/// <summary>Newtons.</summary>
type Newtons = Product<Kilograms, MetersPerSecondSquared>
/// <category>Unit</category>
/// <summary>Pascals.</summary>
type Pascals = Rate<Newtons, SquareMeters>
/// <category>Unit</category>
/// <summary>Kilograms per cubic meter.</summary>
type KilogramsPerCubicMeter = Rate<Kilograms, CubicMeters>
/// <category>Unit</category>
/// <summary>Joules.</summary>
type Joules = Product<Newtons, Meters>


// ---- Light

/// <category>Unit</category>
/// <summary>
/// <a href="https://en.wikipedia.org/wiki/Candela">Candelas</a>
/// are a measure of <see cref="T:Math.Units.LuminousIntensity"/>
/// measured in <see cref="T:Math.Units.Lumens"/> per
/// <see cref="T:Math.Units.Steradians"/>.
/// </summary>
type Candelas = Rate<Lumens, Steradians>
/// <category>Unit</category>
/// <summary>Lux.</summary>
type Lux = Rate<Lumens, SquareMeters>
/// <category>Unit</category>
/// <summary>Nits.</summary>
type Nits = Rate<Candelas, SquareMeters>


// ---- Atomic

/// <category>Unit</category>
/// <summary>Moles per cubic meter.</summary>
type MolesPerCubicMeter = Rate<Moles, CubicMeters>


// ---- Electrical

/// <category>Unit</category>
/// <summary>Watts.</summary>
type Watts = Rate<Joules, Seconds>
/// <category>Unit</category>
/// <summary>Amperes.</summary>
type Amperes = Rate<Coulombs, Seconds>
/// <category>Unit</category>
/// <summary>Volts.</summary>
type Volts = Rate<Watts, Amperes>
/// <category>Unit</category>
/// <summary>Farads.</summary>
type Farads = Rate<Coulombs, Volts>
/// <category>Unit</category>
/// <summary>Henries.</summary>
type Henries = Rate<Volts, Rate<Amperes, Seconds>>
/// <category>Unit</category>
/// <summary>Ohms.</summary>
type Ohms = Rate<Volts, Amperes>


// ---- Pixels


/// <category>Unit</category>
/// <summary>Pixels per second.</summary>
type PixelsPerSecond = Rate<Pixel, Seconds>
/// <category>Unit</category>
/// <summary>Pixels per second squared.</summary>
type PixelsPerSecondSquared = Rate<PixelsPerSecond, Seconds>
/// <category>Unit</category>
/// <summary>Square pixels.</summary>
type SquarePixels = Squared<Pixel>


// ---- Quantity Declaration ---------------------------------------------------

/// <category>Unit System</category>
type IQuantity =
    /// <summary>The underlying floating point value.</summary>
    abstract member Value : float

/// <category>Unit System</category>
/// <summary>
/// A <c>Quantity</c> is effectively a <c>number</c> (an <c>Int</c> or <c>Float</c>) tagged with a
/// <c>units</c> type. So a
///     Quantity Float Meters
/// is a <c>Float</c> number of <c>Meters</c> and a
///     Quantity Int Pixels
/// </summary>
/// <param name="quantity">The underlying floating point value.</param>
[<Struct; CustomEquality; CustomComparison>]
type Quantity<'Units>(quantity: float) =
    interface IQuantity with
        /// <summary>The underlying floating point value.</summary>
        member this.Value = quantity

    interface IComparable<Quantity<'Units>> with
        /// <param name="quantity">The other quantity to compare to.</param>
        member this.CompareTo(quantity: Quantity<'Units>) : int = this.Comparison(quantity)

    interface IComparable with
        /// <param name="obj">The object to compare to.</param>
        member this.CompareTo(obj) =
            match obj with
            | :? Quantity<'Units> as quantity -> this.Comparison(quantity)
            | _ -> -1


    // ---- Base Properties & Functions ----

    /// <summary>The underlying floating point value.</summary>
    member this.Value = quantity

    override this.ToString() =
#if FABLE_COMPILER
        $"{quantity}"
#else
        $"{quantity} {typeof<'Units>.Name}"
#endif


    // ---- IComparable Implementation ----

    /// <param name="other">The other quantity.</param>
    member this.Comparison(other: Quantity<'Units>) =
        if this.Value = other.Value then 0
        elif this.Value < other.Value then -1
        else 1


    /// <param name="other">The other quantity.</param>
    member this.Equals(other: Quantity<'Units>) : bool =
        this.Value = other.Value


    /// <param name="other">The other quantity.</param>
    member this.LessThan(other: Quantity<'Units>) = this.Value < other.Value


    override this.GetHashCode() =
        hash this.Value


    /// <param name="obj">The object to compare.</param>
    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Quantity<'Units> as other -> this.Equals(other)
        | _ -> false


    // ---- Built In Functions ----

    /// <param name="q">The quantity.</param>
    static member Abs(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(abs q.Value)

    /// <param name="lhs">The first quantity.</param>
    /// <param name="rhs">The second quantity.</param>
    static member Min(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(min lhs.Value rhs.Value)

    /// <param name="lhs">The first quantity.</param>
    /// <param name="rhs">The second quantity.</param>
    static member Max(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(max lhs.Value rhs.Value)

    /// <param name="value">The squared quantity.</param>
    static member Sqrt(value: Quantity<'Units Squared>) : Quantity<'Units> = Quantity<'Units>(sqrt value.Value)

    /// <param name="value">The quantity.</param>
    static member Floor(value: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(floor value.Value)

    /// <param name="q">The quantity.</param>
    static member Ceiling(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(ceil q.Value)

    /// <param name="q">The quantity.</param>
    static member Round(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(round q.Value)

    /// <param name="q">The quantity.</param>
    static member Truncate(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(truncate q.Value)


    // ---- Operators ----

    /// <param name="lhs">The first quantity.</param>
    /// <param name="rhs">The second quantity.</param>
    static member (+)(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(lhs.Value + rhs.Value)

    /// <param name="lhs">The first quantity.</param>
    /// <param name="rhs">The second quantity.</param>
    static member (-)(lhs: Quantity<'Units>, rhs: Quantity<'Units>) : Quantity<'Units> =
        Quantity<'Units>(lhs.Value - rhs.Value)

    /// <param name="q">The quantity.</param>
    static member (~-)(q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(-q.Value)

    /// <param name="q">The quantity.</param>
    /// <param name="scale">The scaling factor.</param>
    static member (*)(q: Quantity<'Units>, scale: float) : Quantity<'Units> = Quantity<'Units>(q.Value * scale)

    /// <param name="scale">The scaling factor.</param>
    /// <param name="q">The quantity.</param>
    static member (*)(scale: float, q: Quantity<'Units>) : Quantity<'Units> = Quantity<'Units>(q.Value * scale)

    /// <param name="lhs">The first quantity.</param>
    /// <param name="rhs">The second quantity.</param>
    static member (*)(lhs: Quantity<'UnitA>, rhs: Quantity<'UnitB>) : Quantity<Product<'UnitA, 'UnitB>> =
        Quantity<Product<'UnitA, 'UnitB>>(lhs.Value * rhs.Value)

    /// <param name="q">The quantity.</param>
    /// <param name="scale">The divisor.</param>
    static member (/)(q: Quantity<'Units>, scale: float) : Quantity<'Units> = Quantity<'Units>(q.Value / scale)

    /// <param name="q">The numerator.</param>
    /// <param name="scale">The denominator.</param>
    static member (/)(q: Quantity<'Units>, scale: Quantity<'Units>) : float = q.Value / scale.Value

    /// <param name="dependent">The dependent quantity.</param>
    /// <param name="independent">The independent quantity.</param>
    static member (/)
        (
            dependent: Quantity<'Dependent>,
            independent: Quantity<'Independent>
        ) : Quantity<Rate<'Dependent, 'Independent>> =
        Quantity(dependent.Value / independent.Value)

    /// <param name="q">The quantity.</param>
    /// <param name="modulus">The modulus.</param>
    static member (%)(q: Quantity<'Units>, modulus: Quantity<'Units>) : Quantity<'Units> =
        Quantity(q.Value % modulus.Value)


// ---- Quantity Types ---------------------------------------------------------


/// <category>Unit System</category>
/// A percentage value. The default range for percentages is 0 to 1 but can also be given in the range 0 to 100.
type Percent = Quantity<Percentage>

/// <category>Unit System</category>
type Duration = Quantity<Seconds>


// ---- Distance

/// <category>Unit System</category>
type Length = Quantity<Meters>
/// <category>Unit System</category>
type Area = Quantity<SquareMeters>
/// <category>Unit System</category>
type Volume = Quantity<CubicMeters>
/// <category>Unit System</category>
type Pixels = Quantity<Pixel>
/// <category>Unit System</category>
type Speed = Quantity<MetersPerSecond>
/// <category>Unit System</category>
type Acceleration = Quantity<MetersPerSecondSquared>


// ---- Angular


/// <category>Unit System</category>
type Angle = Quantity<Radians>
/// <category>Unit System</category>
type AngularSpeed = Quantity<RadiansPerSecond>
/// <category>Unit System</category>
type AngularAcceleration = Quantity<RadiansPerSecondSquared>
/// <category>Unit System</category>
type SolidAngle = Quantity<Steradians>


// ---- Mass


/// <category>Unit System</category>
type Mass = Quantity<Kilograms>
/// <category>Unit System</category>
type Density = Quantity<KilogramsPerCubicMeter>
/// <category>Unit System</category>
type Force = Quantity<Newtons>
/// <category>Unit System</category>
type Energy = Quantity<Joules>
/// <category>Unit System</category>
type Pressure = Quantity<Pascals>

// ---- Light


/// <category>Unit System</category>
type LuminousFlux = Quantity<Lumens>
/// <category>Unit System</category>
type LuminousIntensity = Quantity<Candelas>
/// <category>Unit System</category>
type Illuminance = Quantity<Lux>
/// <category>Unit System</category>
type Luminance = Quantity<Nits>

// ---- Atomic


/// <category>Unit System</category>
type SubstanceAmount = Quantity<Moles>
/// <category>Unit System</category>
type Molarity = Quantity<MolesPerCubicMeter>

// ---- Electrical


/// <category>Unit System</category>
type Charge = Quantity<Coulombs>
/// <category>Unit System</category>
type Current = Quantity<Amperes>
/// <category>Unit System</category>
type Capacitance = Quantity<Farads>
/// <category>Unit System</category>
type Inductance = Quantity<Henries>
/// <category>Unit System</category>
type Power = Quantity<Watts>
/// <category>Unit System</category>
type Resistance = Quantity<Ohms>
/// <category>Unit System</category>
type Voltage = Quantity<Volts>

// ---- Temperature ------------------------------------------------------------


/// <category>Unit System</category>
type TemperatureDelta = Quantity<CelsiusDegrees>

/// <category>Unit System</category>
/// <param name="kelvin">The absolute temperature in Kelvins.</param>
[<Struct; CustomEquality; CustomComparison>]
type Temperature(kelvin: float) =
    interface IComparable<Temperature> with
        /// <param name="temp">The other temperature.</param>
        member this.CompareTo(temp: Temperature) : int = this.Comparison(temp)

    interface IComparable with
        /// <param name="obj">The object to compare to.</param>
        member this.CompareTo(obj) =
            match obj with
            | :? Temperature as temp -> this.Comparison(temp)
            | _ -> -1


    // ---- Base Properties & Functions ----

    /// <summary>The absolute temperature in Kelvins.</summary>
    member this.Value = kelvin

    override this.ToString() = $"{kelvin}K"


    // ---- IComparable Implementation ----

    /// <param name="other">The other temperature.</param>
    member this.Comparison(other: Temperature) =
        if this.Value = other.Value then 0
        elif this.Value < other.Value then -1
        else 1


    /// <param name="other">The other temperature.</param>
    member this.Equals(other: Temperature) : bool =
        this.Value = other.Value


    /// <param name="other">The other temperature.</param>
    member this.LessThan(other: Temperature) = this.Value < other.Value


    override this.GetHashCode() =
        hash this.Value


    /// <param name="obj">The object to compare.</param>
    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Temperature as other -> this.Equals(other)
        | _ -> false


    // ---- Built In Functions ----

    /// <param name="lhs">The first temperature.</param>
    /// <param name="rhs">The second temperature.</param>
    static member Min(lhs: Temperature, rhs: Temperature) : Temperature = Temperature(min lhs.Value rhs.Value)

    /// <param name="lhs">The first temperature.</param>
    /// <param name="rhs">The second temperature.</param>
    static member Max(lhs: Temperature, rhs: Temperature) : Temperature = Temperature(max lhs.Value rhs.Value)

    /// <param name="value">The temperature to round down.</param>
    static member Floor(value: Temperature) : Temperature = Temperature(floor value.Value)

    /// <param name="q">The temperature to round up.</param>
    static member Ceiling(q: Temperature) : Temperature = Temperature(ceil q.Value)

    /// <param name="q">The temperature to round.</param>
    static member Round(q: Temperature) : Temperature = Temperature(round q.Value)

    /// <param name="q">The temperature to truncate.</param>
    static member Truncate(q: Temperature) : Temperature = Temperature(truncate q.Value)


    // ---- Operators ----

    /// <param name="lhs">The absolute temperature.</param>
    /// <param name="rhs">The change in temperature.</param>
    static member (+)(lhs: Temperature, rhs: TemperatureDelta) : Temperature = Temperature(lhs.Value + rhs.Value)

    /// <param name="lhs">The change in temperature.</param>
    /// <param name="rhs">The absolute temperature.</param>
    static member (+)(lhs: TemperatureDelta, rhs: Temperature) : Temperature = Temperature(lhs.Value + rhs.Value)

    /// <param name="lhs">The first temperature.</param>
    /// <param name="rhs">The second temperature.</param>
    static member (-)(lhs: Temperature, rhs: Temperature) : TemperatureDelta = TemperatureDelta(lhs.Value - rhs.Value)

    /// <param name="lhs">The absolute temperature.</param>
    /// <param name="rhs">The change in temperature.</param>
    static member (-)(lhs: Temperature, rhs: TemperatureDelta) : Temperature = Temperature(lhs.Value - rhs.Value)


// ---- Interval ---------------------------------------------------------------


/// <category>Unit Relation</category>
/// <summary>
/// A finite, closed interval with a minimum and maximum number. This can
/// represent an interval of any type.
///
/// For example...
/// <code>
///     Interval float
///     Interval int
///     Interval Angle
/// </code>
/// </summary>
[<CustomEquality; NoComparison>]
type Interval<'Units> =
    /// <param name="Start">The start of the interval.</param>
    /// <param name="Finish">The end of the interval.</param>
    | Interval of Start: Quantity<'Units> * Finish: Quantity<'Units>

    /// <param name="obj">The object to compare.</param>
    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Interval<'Units> as other ->
            match this, other with
            | Interval(thisStart, thisFinish), Interval(otherStart, otherFinish) ->
                thisStart = otherStart && thisFinish = otherFinish

        | _ -> false

    override this.ToString() =
        match this with
        | Interval(start, finish) -> $"Interval [ {start} -> {finish} ]"

    override this.GetHashCode() : int =
        match this with
        | Interval(start, finish) -> hash (start, finish)
