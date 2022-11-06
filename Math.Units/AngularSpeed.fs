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

/// Alias for <c>1</c.
let revolutionsPerSecond = turnsPerSecond

/// Alias for <c>1</c.
let inRevolutionsPerSecond =
    inTurnsPerSecond

/// Alias for <c>1</c>.
let revolutionsPerMinute = turnsPerMinute

/// Alias for <c>inTurnsPerMinute</c>.
let inRevolutionsPerMinute =
    inTurnsPerMinute
