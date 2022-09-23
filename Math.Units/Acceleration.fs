/// An `Acceleration` represents an acceleration in meters per second squared,
/// feet per second squared or [gees][1]. It is stored as a number of meters per
/// second squared.
/// Note that since `MetersPerSecondSquared` is defined as `Rate MetersPerSecond
/// Seconds` (change in speed per unit time), you can construct an `Acceleration`
/// value using `Quantity.per`:
///     acceleration =
///         changeInSpeed |> Quantity.per duration
/// You can also do rate-related calculations with `Acceleration` values to compute
/// `Speed` or `Duration`:
///     changeInSpeed =
///         acceleration |> Quantity.for duration
///     alsoChangeInSpeed =
///         duration |> Quantity.at acceleration
///     duration =
///         changeInSpeed |> Quantity.at_ acceleration
///
/// [1]: https://en.wikipedia.org/wiki/G-force#Unit_and_measurement
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Acceleration

/// Construct an acceleration from a number of meters per second squared.
let metersPerSecondSquared (numMetersPerSecondSquared: float) : Acceleration = Quantity numMetersPerSecondSquared

/// Convert an acceleration to a number of meters per second squared.
let inMetersPerSecondSquared (numMetersPerSecondSquared: Acceleration) : float = numMetersPerSecondSquared.Value

/// Construct an acceleration from a number of feet per second squared.
let feetPerSecondSquared (numFeetPerSecondSquared: float) : Acceleration =
    metersPerSecondSquared (Constants.foot * numFeetPerSecondSquared)

/// Convert an acceleration to a number of feet per second squared.
let inFeetPerSecondSquared (acceleration: Acceleration) : float =
    inMetersPerSecondSquared acceleration
    / Constants.foot

/// Construct an acceleration from a number of [gees][1]. One gee is equal to
/// 9.80665 meters per second squared (the standard acceleration due to gravity).
///     Acceleration.gees 1
///     --> Acceleration.metersPerSecondSquared 9.80665
/// [1]: https://en.wikipedia.org/wiki/G-force#Unit_and_measurement
let gees (numGees: float) : Acceleration =
    metersPerSecondSquared (Constants.gee * numGees)

/// Convert an acceleration to a number of gees.
let inGees (acceleration: Acceleration) : float =
    inMetersPerSecondSquared acceleration
    / Constants.gee
