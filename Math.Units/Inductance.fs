/// <category>Module: Unit System</category>
/// <summary>
/// An <c>Inductance</c> value represents an electrical inductance in henries.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Inductance

/// <category>Conversions</category>
/// <summary>Construct an inductance from a number of henries.</summary>
/// <param name="numHenries">The number of henries.</param>
let henries (numHenries: float) : Inductance = Quantity numHenries

/// <category>Conversions</category>
/// <summary>Convert an inductance to a number of henries.</summary>
/// <param name="inductance">The inductance to convert.</param>
let inHenries (inductance: Inductance) : float = inductance.Value

/// <category>Conversions</category>
/// <summary>Construct an inductance from a number of millihenries.</summary>
/// <param name="numMilliHenries">The number of millihenries.</param>
let millihenries (numMilliHenries: float) : Inductance = henries (numMilliHenries * 1.0e-3)

/// <category>Conversions</category>
/// <summary>Convert an inductance to a number of millihenries.</summary>
/// <param name="inductance">The inductance to convert.</param>
let inMillihenries (inductance: Inductance) : float = inHenries inductance / 1.0e-3

/// <category>Conversions</category>
/// <summary>Construct an inductance from a number of microhenries.</summary>
/// <param name="numMicroHenries">The number of microhenries.</param>
let microhenries (numMicroHenries: float) : Inductance = henries (numMicroHenries * 1.0e-6)

/// <category>Conversions</category>
/// <summary>Convert an inductance to a number of microhenries.</summary>
/// <param name="inductance">The inductance to convert.</param>
let inMicrohenries (inductance: Inductance) : float = inHenries inductance / 1.0e-6

/// <category>Conversions</category>
/// <summary>Construct an inductance from a number of nanohenries.</summary>
/// <param name="numNanoHenries">The number of nanohenries.</param>
let nanohenries (numNanoHenries: float) : Inductance = henries (numNanoHenries * 1.0e-9)

/// <category>Conversions</category>
/// <summary>Convert an inductance to a number of nanohenries.</summary>
/// <param name="inductance">The inductance to convert.</param>
let inNanohenries (inductance: Inductance) : float = inHenries inductance / 1.0e-9

/// <category>Conversions</category>
/// <summary>Construct an inductance from a number of kilohenries.</summary>
/// <param name="numKiloHenries">The number of kilohenries.</param>
let kilohenries (numKiloHenries: float) : Inductance = henries (numKiloHenries * 1.0e3)

/// <category>Conversions</category>
/// <summary>Convert an inductance to a number of kilohenries.</summary>
/// <param name="inductance">The inductance to convert.</param>
let inKilohenries (inductance: Inductance) : float = inHenries inductance / 1.0e3
