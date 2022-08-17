[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Acceleration

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
