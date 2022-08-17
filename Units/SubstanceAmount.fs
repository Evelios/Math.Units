/// A `SubstanceAmount` value represents a substance amount in [moles][1].
/// [1]: https://en.wikipedia.org/wiki/Mole_(unit)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.SubstanceAmount

/// Construct a substance amount from a number of moles.
let moles (numMoles: float) : SubstanceAmount = Quantity numMoles

/// Convert a substance amount to a number of moles.
let inMoles (numMoles: SubstanceAmount) : float = numMoles.Value

/// Construct a substance amount from a number of picomoles.
let picomoles (numPicomoles: float) : SubstanceAmount = moles (numPicomoles * 1.0e-12)

/// Convert a substance amount to a number of picomoles.
let inPicomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-12

/// Construct a substance amount from a number of nanomoles.
let nanomoles (numNanomoles: float) : SubstanceAmount = moles (numNanomoles * 1.0e-9)

/// Convert a substance amount to a number of nanomoles.
let inNanomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-9

/// Construct a substance amount from a number of micromoles.
let micromoles (numMicromoles: float) : SubstanceAmount = moles (numMicromoles * 1.0e-6)

/// Convert a substance amount to a number of micromoles.
let inMicromoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-6

/// Construct a substance amount from a number of millimoles.
let millimoles (numMillimoles: float) : SubstanceAmount = moles (numMillimoles * 1.0e-3)

/// Convert a substance amount to a number of millimoles.
let inMillimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-3

/// Construct a substance amount from a number of centimoles.
let centimoles (numCentimoles: float) : SubstanceAmount = moles (numCentimoles * 1.0e-2)

/// Convert a substance amount to a number of centimoles.
let inCentimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-2

/// Construct a substance amount from a number of decimoles.
let decimoles (numDecimoles: float) : SubstanceAmount = moles (numDecimoles * 1.0e-1)

/// Convert a substance amount to a number of decimoles.
let inDecimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-1

/// Construct a substance amount from a number of kilomoles.
let kilomoles (numKilomoles: float) : SubstanceAmount = moles (numKilomoles * 1.0e3)

/// Convert a substance amount to a number of kilomoles.
let inKilomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e3

/// Construct a substance amount from a number of megamoles.
let megamoles (numMegamoles: float) : SubstanceAmount = moles (numMegamoles * 1.0e6)

/// Convert a substance amount to a number of megamoles.
let inMegamoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e6

/// Construct a substance amount from a number of gigamoles.
let gigamoles (numGigamoles: float) : SubstanceAmount = moles (numGigamoles * 1.0e9)

/// Convert a substance amount to a number of gigamoles.
let inGigamoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e9
