/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.AngularSpeed

open System

/// Construct an angular speed from a number of radians per second.
let radiansPerSecond (numRadiansPerSecond: float) : AngularSpeed = Quantity numRadiansPerSecond

/// Convert an angular speed to a number of radians per second.
let inRadiansPerSecond (numRadiansPerSecond: AngularSpeed) : float = numRadiansPerSecond.Value

/// Construct an angular speed from a number of degrees per second.
let degreesPerSecond (numDegreesPerSecond: float) : AngularSpeed =
    radiansPerSecond (Math.PI / 180. * numDegreesPerSecond)

/// Convert an angular speed to a number of degrees per second.
let inDegreesPerSecond (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed / (Math.PI / 180.)

/// Construct an angular speed from a number of turns per second.
let turnsPerSecond (numTurnsPerSecond: float) : AngularSpeed =
    radiansPerSecond (2. * Math.PI * numTurnsPerSecond)

/// Convert an angular speed to a number of turns per second.
let inTurnsPerSecond (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed / (2. * Math.PI)

/// Construct an angular speed from a number of turns per minute.
let turnsPerMinute (numTurnsPerMinute: float) : AngularSpeed =
    radiansPerSecond ((2. * Math.PI * numTurnsPerMinute) / 60.)

/// Convert an angular speed to a number of turns per minute.
let inTurnsPerMinute (angularSpeed: AngularSpeed) : float =
    inRadiansPerSecond angularSpeed
    / ((2. * Math.PI) / 60.)

// ---- Function Aliases -------------------------------------------------------

/// <summary>
/// Alias for <c>AngularSpeed.turnsPerSecond</c>.
/// </summary>
let revolutionsPerSecond = turnsPerSecond

/// <summary>
/// Alias for <c>AngularSpeed.inTurnsPerSecond</c>.
/// </summary>
let inRevolutionsPerSecond =
    inTurnsPerSecond

/// <summary>
/// Alias for <c>AngularSpeed.turnsPerMinute</c>.
/// </summary>
let revolutionsPerMinute = turnsPerMinute

/// <summary>
/// Alias for <c>AngularSpeed.inTurnsPerMinute</c>.
/// </summary>
let inRevolutionsPerMinute =
    inTurnsPerMinute
