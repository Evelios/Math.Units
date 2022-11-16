/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Charge

/// Construct a charge from a number of coulombs.
let coulombs (numCoulombs: float) : Charge = Quantity numCoulombs

/// Convert a charge to a number of coulombs.
let inCoulombs (numCoulombs: Charge) : float = numCoulombs.Value

/// Construct a charge from a number of ampere hours.
let ampereHours (numAmpereHours: float) : Charge =
    coulombs (Constants.hour * numAmpereHours)

/// Convert a charge to a number of ampere hours.
let inAmpereHours (charge: Charge) : float = inCoulombs charge / Constants.hour

/// Construct a charge from a number of milliampere hours.
let milliampereHours (numMilliampereHours: float) : Charge =
    coulombs (Constants.hour * numMilliampereHours / 1000.)

/// Convert a charge to a number of milliampere hours.
let inMilliampereHours (charge: Charge) : float =
    inCoulombs charge * 1000. / Constants.hour
