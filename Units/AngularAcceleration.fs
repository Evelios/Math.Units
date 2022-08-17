[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.AngularAcceleration

open System

/// Construct an angular acceleration from a number of radians per second squared.
let radiansPerSecondSquared (numRadiansPerSecondSquared: float) : AngularAcceleration =
    Quantity numRadiansPerSecondSquared

/// Convert an angular acceleration to a number of radians per second squared.
let inRadiansPerSecondSquared (numRadiansPerSecondSquared: AngularAcceleration) : float =
    numRadiansPerSecondSquared.Value

/// Construct an angular acceleration from a number of degrees per second squared.
let degreesPerSecondSquared (numDegreesPerSecondSquared: float) : AngularAcceleration =
    radiansPerSecondSquared (Math.PI / 180. * numDegreesPerSecondSquared)

/// Convert an angular acceleration to a number of degrees per second squared.
let inDegreesPerSecondSquared (angularAcceleration: AngularAcceleration) =
    inRadiansPerSecondSquared angularAcceleration
    / (Math.PI / 180.)

/// Construct an angular acceleration from a number of turns per second squared.
let turnsPerSecondSquared (numTurnsPerSecondSquared: float) : AngularAcceleration =
    radiansPerSecondSquared (2. * Math.PI * numTurnsPerSecondSquared)

/// Convert an angular acceleration to a number of turns per second squared.
let inTurnsPerSecondSquared (angularAcceleration: AngularAcceleration) : float =
    inRadiansPerSecondSquared angularAcceleration
    / (2. * Math.PI)
