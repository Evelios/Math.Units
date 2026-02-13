/// <category>Module: Unit System</category>
/// <summary>
/// A <c>SubstanceAmount</c> value represents a substance amount in
/// <a href="https://en.wikipedia.org/wiki/Mole_(unit)">moles</a>.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.SubstanceAmount

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of moles.</summary>
/// <param name="numMoles">The number of moles.</param>
let moles (numMoles: float) : SubstanceAmount = Quantity numMoles

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of moles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inMoles (substanceAmount: SubstanceAmount) : float = substanceAmount.Value

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of picomoles.</summary>
/// <param name="numPicomoles">The number of picomoles.</param>
let picomoles (numPicomoles: float) : SubstanceAmount = moles (numPicomoles * 1.0e-12)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of picomoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inPicomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-12

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of nanomoles.</summary>
/// <param name="numNanomoles">The number of nanomoles.</param>
let nanomoles (numNanomoles: float) : SubstanceAmount = moles (numNanomoles * 1.0e-9)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of nanomoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inNanomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-9

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of micromoles.</summary>
/// <param name="numMicromoles">The number of micromoles.</param>
let micromoles (numMicromoles: float) : SubstanceAmount = moles (numMicromoles * 1.0e-6)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of micromoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inMicromoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-6

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of millimoles.</summary>
/// <param name="numMillimoles">The number of millimoles.</param>
let millimoles (numMillimoles: float) : SubstanceAmount = moles (numMillimoles * 1.0e-3)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of millimoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inMillimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-3

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of centimoles.</summary>
/// <param name="numCentimoles">The number of centimoles.</param>
let centimoles (numCentimoles: float) : SubstanceAmount = moles (numCentimoles * 1.0e-2)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of centimoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inCentimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-2

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of decimoles.</summary>
/// <param name="numDecimoles">The number of decimoles.</param>
let decimoles (numDecimoles: float) : SubstanceAmount = moles (numDecimoles * 1.0e-1)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of decimoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inDecimoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e-1

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of kilomoles.</summary>
/// <param name="numKilomoles">The number of kilomoles.</param>
let kilomoles (numKilomoles: float) : SubstanceAmount = moles (numKilomoles * 1.0e3)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of kilomoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inKilomoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e3

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of megamoles.</summary>
/// <param name="numMegamoles">The number of megamoles.</param>
let megamoles (numMegamoles: float) : SubstanceAmount = moles (numMegamoles * 1.0e6)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of megamoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inMegamoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e6

/// <category>Conversions</category>
/// <summary>Construct a substance amount from a number of gigamoles.</summary>
/// <param name="numGigamoles">The number of gigamoles.</param>
let gigamoles (numGigamoles: float) : SubstanceAmount = moles (numGigamoles * 1.0e9)

/// <category>Conversions</category>
/// <summary>Convert a substance amount to a number of gigamoles.</summary>
/// <param name="substanceAmount">The substance amount to convert.</param>
let inGigamoles (substanceAmount: SubstanceAmount) : float = inMoles substanceAmount / 1.0e9
