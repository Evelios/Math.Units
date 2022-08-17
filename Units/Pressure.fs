/// A `Pressure` value represents a pressure in kilopascals, pounds per square
/// inch, [atmospheres][1] etc. It is stored as a number of pascals.
/// Note that since `Pascals` is defined as `Rate Newtons SquareMeters` (force per
/// unit area), you can construct a `Pressure` value using `Quantity.per`:
///     pressure =
///         force |> Quantity.per area
/// You can also do rate-related calculations with `Pressure` values to compute
/// `Force` or `Area`:
///     force =
///         area |> Quantity.at pressure
///     area =
///         force |> Quantity.at_ pressure
/// [1]: https://en.wikipedia.org/wiki/Atmosphere_(unit)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Pressure

/// Construct a pressure from a number of pascals.
let pascals (numPascals: float) : Pressure = Quantity numPascals

/// Convert a pressure to a number of pascals.
let inPascals (numPascals: Pressure) : float = numPascals.Value

/// Construct a pressure from a number of kilopascals.
let kilopascals (numKilopascals: float) : Pressure = pascals (1000. * numKilopascals)

/// Convert a pressure to a number of kilopascals.
let inKilopascals (pressure: Pressure) : float = inPascals pressure / 1000.

/// Construct a pressure from a number of megapascals.
let megapascals (numMegapascals: float) : Pressure = pascals (1.0e6 * numMegapascals)

/// Convert a pressure to a number of megapascals.
let inMegapascals (pressure: Pressure) : float = inPascals pressure / 1.0e6

/// Construct a pressure from a number of pounds per square inch.
let poundsPerSquareInch (value: float) : Pressure =
    Force.pounds value
    |> Quantity.per (Area.squareInches 1.)

/// Convert a pressure to a number of pounds per square inch.
let inPoundsPerSquareInch (pressure: Pressure) : float =
    Area.squareInches 1.
    |> Quantity.at pressure
    |> Force.inPounds

/// Construct a pressure from a number of [atmospheres][1].
/// [1]: https://en.wikipedia.org/wiki/Atmosphere_(unit)
let atmospheres (numAtmospheres: float) : Pressure =
    pascals (Constants.atmosphere * numAtmospheres)

/// Convert a pressure to a number of atmospheres.
let inAtmospheres (pressure: Pressure) : float =
    inPascals pressure / Constants.atmosphere
