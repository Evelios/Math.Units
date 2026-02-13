/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Speed</c> value represents a speed in meters per second, miles per hour etc.
/// It is stored as a number of meters per second.
/// </summary>
/// 
/// <note>
/// Since <c>MetersPerSecond</c> is defined as <c>Rate Meters Seconds</c> (length
/// per unit time), you can construct a <c>Speed</c> value using <c>Quantity.per</c>:
/// <code>
///     let speed =
///         length |&gt; Quantity.per duration
/// </code>
/// 
/// You can also do rate-related calculations with <c>Speed</c> values to compute
/// <c>Length</c> or <c>Duration</c>:
/// <code>
///     let length =
///         speed |&gt; Quantity.for duration
///     let alsoLength =
///         duration |&gt; Quantity.at speed
///     let duration =
///         length |&gt; Quantity.at_ speed
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Speed

/// <category>Conversions</category>
/// <summary>Construct a speed from a number of meters per second.</summary>
/// <param name="numMetersPerSecond">The number of meters per second.</param>
let metersPerSecond (numMetersPerSecond: float) : Speed = Quantity numMetersPerSecond

/// <category>Conversions</category>
/// <summary>Convert a speed to a number of meters per second.</summary>
/// <param name="speed">The speed value to convert.</param>
let inMetersPerSecond (speed: Speed) : float = speed.Value

/// <category>Conversions</category>
/// <summary>Construct a speed from a number of feet per second.</summary>
/// <param name="numFeetPerSecond">The number of feet per second.</param>
let feetPerSecond (numFeetPerSecond: float) : Speed =
    metersPerSecond (Constants.foot * numFeetPerSecond)

/// <category>Conversions</category>
/// <summary>Convert a speed to a number of feet per second.</summary>
/// <param name="speed">The speed value to convert.</param>
let inFeetPerSecond (speed: Speed) : float =
    inMetersPerSecond speed / Constants.foot

/// <category>Conversions</category>
/// <summary>Construct a speed from a number of kilometers per hour.</summary>
/// <param name="numKilometersPerHour">The number of kilometers per hour.</param>
let kilometersPerHour (numKilometersPerHour: float) : Speed =
    metersPerSecond (numKilometersPerHour * 1000. / Constants.hour)

/// <category>Conversions</category>
/// <summary>Convert a speed to a number of kilometers per hour.</summary>
/// <param name="speed">The speed value to convert.</param>
let inKilometersPerHour (speed: Speed) : float =
    Constants.hour * inMetersPerSecond speed * 0.001

/// <category>Conversions</category>
/// <summary>Construct a speed from a number of miles per hour.</summary>
/// <param name="numMilesPerHour">The number of miles per hour.</param>
let milesPerHour (numMilesPerHour: float) : Speed =
    metersPerSecond (numMilesPerHour * Constants.mile / Constants.hour)

/// <category>Conversions</category>
/// <summary>Convert a speed to a number of miles per hour.</summary>
/// <param name="speed">The speed value to convert.</param>
let inMilesPerHour (speed: Speed) : float =
    (Constants.hour / Constants.mile)
    * inMetersPerSecond speed
