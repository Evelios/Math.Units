/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Pressure</c> value represents a pressure in kilopascals, pounds per square
/// inch, <a href="https://en.wikipedia.org/wiki/Atmosphere_(unit)">atmospheres</a>
/// etc. It is stored as a number of pascals.
/// </summary>
/// 
/// <note>
/// Since <c>Pascals</c> is defined as <c>Rate Newtons SquareMeters</c> (force per
/// unit area), you can construct a <c>Pressure</c> value using <c>Quantity.per</c>:
///     pressure =
///         force |&gt; Quantity.per area
/// You can also do rate-related calculations with <c>Pressure</c> values to compute
/// <c>Force</c> or <c>Area</c>:
/// <code>
///     let force =
///         area |&gt; Quantity.at pressure
///     let area =
///         force |&gt; Quantity.at_ pressure
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Pressure

/// <category>Metric</category>
/// <summary>Construct a pressure from a number of pascals.</summary>
/// <param name="numPascals">The number of pascals.</param>
let pascals (numPascals: float) : Pressure = Quantity numPascals

/// <category>Metric</category>
/// <summary>Convert a pressure to a number of pascals.</summary>
/// <param name="pressure">The pressure to convert.</param>
let inPascals (pressure: Pressure) : float = pressure.Value

/// <category>Metric</category>
/// <summary>Construct a pressure from a number of kilopascals.</summary>
/// <param name="numKilopascals">The number of kilopascals.</param>
let kilopascals (numKilopascals: float) : Pressure = pascals (1000. * numKilopascals)

/// <category>Metric</category>
/// <summary>Convert a pressure to a number of kilopascals.</summary>
/// <param name="pressure">The pressure to convert.</param>
let inKilopascals (pressure: Pressure) : float = inPascals pressure / 1000.

/// <category>Metric</category>
/// <summary>Construct a pressure from a number of megapascals.</summary>
/// <param name="numMegapascals">The number of megapascals.</param>
let megapascals (numMegapascals: float) : Pressure = pascals (1.0e6 * numMegapascals)

/// <category>Metric</category>
/// <summary>Convert a pressure to a number of megapascals.</summary>
/// <param name="pressure">The pressure to convert.</param>
let inMegapascals (pressure: Pressure) : float = inPascals pressure / 1.0e6

/// <category>Imperial</category>
/// <summary>Construct a pressure from a number of pounds per square inch.</summary>
/// <param name="value">The number of pounds per square inch.</param>
let poundsPerSquareInch (value: float) : Pressure =
    Force.pounds value
    |> Quantity.per (Area.squareInches 1.)

/// <category>Imperial</category>
/// <summary>Convert a pressure to a number of pounds per square inch.</summary>
/// <param name="pressure">The pressure to convert.</param>
let inPoundsPerSquareInch (pressure: Pressure) : float =
    Area.squareInches 1.
    |> Quantity.at pressure
    |> Force.inPounds

/// <category>Atmospheric</category>
/// <summary>
/// Construct a pressure from a number of <a href="https://en.wikipedia.org/wiki/Atmosphere_(unit)">atmospheres</a>.
/// </summary>
/// <param name="numAtmospheres">The number of atmospheres.</param>
let atmospheres (numAtmospheres: float) : Pressure =
    pascals (Constants.atmosphere * numAtmospheres)

/// <category>Atmospheric</category>
/// <summary>Convert a pressure to a number of atmospheres.</summary>
/// <param name="pressure">The pressure to convert.</param>
let inAtmospheres (pressure: Pressure) : float =
    inPascals pressure / Constants.atmosphere
