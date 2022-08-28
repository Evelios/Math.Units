/// A `Speed` value represents a speed in meters per second, miles per hour etc.
/// It is stored as a number of meters per second.
/// Note that since `MetersPerSecond` is defined as `Rate Meters Seconds` (length
/// per unit time), you can construct a `Speed` value using `Quantity.per`:
///     speed =
///         length |> Quantity.per duration
/// You can also do rate-related calculations with `Speed` values to compute
/// `Length` or `Duration`:
///     length =
///         speed |> Quantity.for duration
///     alsoLength =
///         duration |> Quantity.at speed
///     duration =
///         length |> Quantity.at_ speed
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Speed

/// Construct a speed from a number of meters per second.
let metersPerSecond (numMetersPerSecond: float) : Speed = Quantity numMetersPerSecond

/// Convert a speed to a number of meters per second.
let inMetersPerSecond (numMetersPerSecond: Speed) : float = numMetersPerSecond.Value

/// Construct a speed from a number of feet per second.
let feetPerSecond (numFeetPerSecond: float) : Speed =
    metersPerSecond (Constants.foot * numFeetPerSecond)

/// Convert a speed to a number of feet per second.
let inFeetPerSecond (speed: Speed) : float =
    inMetersPerSecond speed / Constants.foot

/// Construct a speed from a number of kilometers per hour.
let kilometersPerHour (numKilometersPerHour: float) : Speed =
    metersPerSecond (numKilometersPerHour * 1000. / Constants.hour)

/// Convert a speed to a number of kilometers per hour.
let inKilometersPerHour (speed: Speed) : float =
    Constants.hour * inMetersPerSecond speed * 0.001

/// Construct a speed from a number of miles per hour.
let milesPerHour (numMilesPerHour: float) : Speed =
    metersPerSecond (numMilesPerHour * Constants.mile / Constants.hour)

/// Convert a speed to a number of miles per hour.
let inMilesPerHour (speed: Speed) : float =
    (Constants.hour / Constants.mile)
    * inMetersPerSecond speed
