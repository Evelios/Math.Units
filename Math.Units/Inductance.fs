/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Inductance

/// Construct an inductance from a number of henries.
let henries (numHenries: float) : Inductance = Quantity numHenries

/// Convert an inductance to a number of henries.
let inHenries (numHenries: Inductance) : float = numHenries.Value

/// Construct an inductance from a number of millihenries.
let millihenries (numMilliHenries: float) : Inductance = henries (numMilliHenries * 1.0e-3)

/// Convert an inductance to a number of millihenries.
let inMillihenries (inductance: Inductance) : float = inHenries inductance / 1.0e-3

/// Construct an inductance from a number of microhenries.
let microhenries (numMicroHenries: float) : Inductance = henries (numMicroHenries * 1.0e-6)

/// Convert an inductance to a number of microhenries.
let inMicrohenries (inductance: Inductance) : float = inHenries inductance / 1.0e-6

/// Construct an inductance from a number of nanohenries.
let nanohenries (numNanoHenries: float) : Inductance = henries (numNanoHenries * 1.0e-9)

/// Convert an inductance to a number of nanohenries.
let inNanohenries (inductance: Inductance) : float = inHenries inductance / 1.0e-9

/// Construct an inductance from a number of kilohenries.
let kilohenries (numKiloHenries: float) : Inductance = henries (numKiloHenries * 1.0e3)

/// Convert an inductance to a number of kilohenries.
let inKilohenries (inductance: Inductance) : float = inHenries inductance / 1.0e3
