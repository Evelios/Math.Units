[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Capacitance

/// Construct capacitance from a number of farads.
let farads (numFarads: float) : Capacitance = Quantity numFarads

/// Convert capacitance to a number of farads.
let inFarads (numFarads: Capacitance) : float = numFarads.Value

/// Construct a capacitance from a number of microfarads.
let microfarads (numMicrofarads: float) = farads (numMicrofarads * 1.0e-6)

/// Convert a capacitance to a number of microfarads
let inMicrofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-6

/// Construct a capacitance from a number of nanofarads
let nanofarads (numNanofarads: float) : Capacitance = farads (numNanofarads * 1.0e-9)

/// Convert a capacitance to a number of nanofarads
let inNanofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-9

/// Construct capacitance from a number of picofarads.
let picofarads (numPicofarads: float) : Capacitance = farads (numPicofarads * 1.0e-12)

/// Convert a capacitance to a number of picofarads.
let inPicofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-12
