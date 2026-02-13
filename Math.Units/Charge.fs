/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Charge</c> value represents an electrical charge in coulombs or ampere
/// hours. It is stored as a number of coulombs.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Charge

/// <category>Conversions</category>
/// <summary>Construct a charge from a number of coulombs.</summary>
/// <param name="numCoulombs">The number of coulombs.</param>
let coulombs (numCoulombs: float) : Charge = Quantity numCoulombs

/// <category>Conversions</category>
/// <summary>Convert a charge to a number of coulombs.</summary>
/// <param name="charge">The charge to convert.</param>
let inCoulombs (charge: Charge) : float = charge.Value

/// <category>Conversions</category>
/// <summary>Construct a charge from a number of ampere hours.</summary>
/// <param name="numAmpereHours">The number of ampere hours.</param>
let ampereHours (numAmpereHours: float) : Charge =
    coulombs (Constants.hour * numAmpereHours)

/// <category>Conversions</category>
/// <summary>Convert a charge to a number of ampere hours.</summary>
/// <param name="charge">The charge to convert.</param>
let inAmpereHours (charge: Charge) : float = inCoulombs charge / Constants.hour

/// <category>Conversions</category>
/// <summary>Construct a charge from a number of milliampere hours.</summary>
/// <param name="numMilliampereHours">The number of milliampere hours.</param>
let milliampereHours (numMilliampereHours: float) : Charge =
    coulombs (Constants.hour * numMilliampereHours / 1000.)

/// <category>Conversions</category>
/// <summary>Convert a charge to a number of milliampere hours.</summary>
/// <param name="charge">The charge to convert.</param>
let inMilliampereHours (charge: Charge) : float =
    inCoulombs charge * 1000. / Constants.hour
