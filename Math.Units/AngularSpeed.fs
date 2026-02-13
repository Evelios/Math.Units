/// <category>Module: Unit System</category>
/// <summary>
/// An <c>AngularSpeed</c> represents a rotation rate in radians per second, degrees
/// per second, turns (revolutions) per second or turns (revolutions) per minute.
/// It is stored as a number of radians per second.
/// </summary>
/// 
/// <note>
/// <para>
/// Since <c>RadiansPerSecond</c> is defined as <c>Rate Radians Seconds</c> (angle
/// per unit time), you can construct an <c>AngularSpeed</c> value using <c>Quantity.per</c>:
/// </para>
/// <code>
///     let angularSpeed =
///         angle |&gt; Quantity.per duration
/// </code>
/// <para>
/// You can also do rate-related calculations with <c>AngularSpeed</c> values to compute
/// <c>Angle</c> or <c>Duration</c>:
/// </para>
/// <code>
///     let angle =
///         angularSpeed |&gt; Quantity.for duration
///     let alsoAngle =
///         duration |&gt; Quantity.at angularSpeed
///     let duration =
///         angle |&gt; Quantity.at_ angularSpeed
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.AngularSpeed

open System

/// <category>Conversions</category>
/// <summary>Construct an angular speed from a number of radians per second.</summary>
/// <param name="numRadiansPerSecond">The number of radians per second.</param>
let radiansPerSecond (numRadiansPerSecond: float) : AngularSpeed = Quantity numRadiansPerSecond

/// <category>Conversions</category>
/// <summary>Convert an angular speed to a number of radians per second.</summary>
/// <param name="angularSpeed">The angular speed to convert.</param>
let inRadiansPerSecond (angularSpeed: AngularSpeed) : float = angularSpeed.Value

/// <category>Conversions</category>
/// <summary>Construct an angular speed from a number of degrees per second.</summary>
/// <param name="numDegreesPerSecond">The number of degrees per second.</param>
let degreesPerSecond (numDegreesPerSecond: float) : AngularSpeed =
    radiansPerSecond (Math.PI / 180. * numDegreesPerSecond)

/// <category>Conversions</category>
/// <summary>Convert an angular speed to a number of degrees per second.</summary>
/// <param name="angularSpeed">The angular speed to convert.</param>
let inDegreesPerSecond (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed / (Math.PI / 180.)

/// <category>Conversions</category>
/// <summary>Construct an angular speed from a number of turns per second.</summary>
/// <param name="numTurnsPerSecond">The number of turns per second.</param>
let turnsPerSecond (numTurnsPerSecond: float) : AngularSpeed =
    radiansPerSecond (2. * Math.PI * numTurnsPerSecond)

/// <category>Conversions</category>
/// <summary>Convert an angular speed to a number of turns per second.</summary>
/// <param name="angularSpeed">The angular speed to convert.</param>
let inTurnsPerSecond (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed / (2. * Math.PI)

/// <category>Conversions</category>
/// <summary>Construct an angular speed from a number of turns per minute.</summary>
/// <param name="numTurnsPerMinute">The number of turns per minute.</param>
let turnsPerMinute (numTurnsPerMinute: float) : AngularSpeed =
    radiansPerSecond ((2. * Math.PI * numTurnsPerMinute) / 60.)

/// <category>Conversions</category>
/// <summary>Convert an angular speed to a number of turns per minute.</summary>
/// <param name="angularSpeed">The angular speed to convert.</param>
let inTurnsPerMinute (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed
    / ((2. * Math.PI) / 60.)

// ---- Function Aliases -------------------------------------------------------

/// <category>Conversions</category>
/// <summary>
/// Alias for <c>AngularSpeed.turnsPerSecond</c>.
/// </summary>
let revolutionsPerSecond = turnsPerSecond

/// <category>Conversions</category>
/// <summary>
/// Alias for <c>AngularSpeed.inTurnsPerSecond</c>.
/// </summary>
let inRevolutionsPerSecond =
    inTurnsPerSecond

/// <category>Conversions</category>
/// <summary>
/// Alias for <c>AngularSpeed.turnsPerMinute</c>.
/// </summary>
let revolutionsPerMinute = turnsPerMinute

/// <category>Conversions</category>
/// <summary>
/// Alias for <c>AngularSpeed.inTurnsPerMinute</c>.
/// </summary>
let inRevolutionsPerMinute =
    inTurnsPerMinute
