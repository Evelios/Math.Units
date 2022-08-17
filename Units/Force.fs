[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Force

/// Construct a force value from a number of newtons.
let newtons (numNewtons: float) : Force = Quantity numNewtons

/// Convert a force value to a number of newtons.
let inNewtons (numNewtons: Force) : float = numNewtons.Value

/// Construct a force value from a number of kilonewtons.
let kilonewtons (numKilonewtons: float) : Force = newtons (1000. * numKilonewtons)

/// Convert a force value to a number of kilonewtons.
let inKilonewtons (force: Force) : float = inNewtons force / 1000.

/// Construct a force value from a number of meganewtons.
let meganewtons (numMeganewtons: float) : Force = newtons (1.0e6 * numMeganewtons)

/// Convert a force value to a number of meganewtons.
let inMeganewtons (force: Force) : float = inNewtons force / 1.0e6

/// Construct a force value from a number of pounds force. One pound force is
/// the force required to accelerate one [pound mass][1] at a rate of [one gee][2].
/// [1]: Mass#pounds
/// [2]: Acceleration#gees
let pounds (numPounds: float) : Force =
    newtons (Constants.poundForce * numPounds)

/// Convert a force value to a number of pounds force.
let inPounds (force: Force) : float = inNewtons force / Constants.poundForce

/// Construct a force value from a number of kips (kilopounds force).
///    Force.kips 2
///    --> Force.pounds 2000
let kips (numKips: float) : Force = pounds (1000. * numKips)

/// Convert a force value to a number of kips.
let inKips (force: Force) : float = inPounds force / 1000.
