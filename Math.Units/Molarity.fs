/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Molarity</c> value represents a concentration of substance in moles per
/// cubic meter, moles per liter, millimoles per liter etc. It is stored as a number
/// of moles per cubic meter.
/// </summary>
/// 
/// <note>
/// The <a href="https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-8">NIST Guide to the SI</a>
/// states that the term "molarity" is considered obsolete, but it appears to still
/// be in common use and is far less verbose than the alternative NIST suggestion of
/// "amount-of-substance concentration".
/// Since the units of <c>Molarity</c> are defined to be <c>Rate Moles CubicMeters</c> (amount
/// of substance per unit volume), you can construct a <c>Molarity</c> value using
/// <c>Quantity.per</c>:
/// <code>
///     molarity =
///         substanceAmount |&gt; Quantity.per volume
/// </code>
/// </note>
///
/// <note>
/// You can also do rate-related calculations with <c>Molarity</c> values to compute
/// <c>SubstanceAmount</c> or <c>Volume</c>:
/// <code>
///     let substanceAmount =
///         volume |&gt; Quantity.at molarity
///     let volume =
///         substanceAmount |&gt; Quantity.at_ molarity
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Molarity


// ---- Constants --------------------------------------------------------------

/// <category>Constants</category>
/// One mole per liter, in moles per cubic meter
let oneMolePerLiter =
    Constants.mole / Constants.liter

/// <category>Constants</category>
/// One decimole per liter, in moles per cubic meter
let oneDecimolePerLiter =
    0.1 * Constants.mole / Constants.liter


// ---- Conversions ------------------------------------------------------------

/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of moles per cubic meter.</summary>
/// <param name="numMolesPerCubicMeter">The number of moles per cubic meter.</param>
let molesPerCubicMeter (numMolesPerCubicMeter: float) : Molarity = Quantity numMolesPerCubicMeter

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of moles per cubic meter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inMolesPerCubicMeter (molarity: Molarity) : float = molarity.Value

/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of moles per liter.</summary>
/// <param name="numMolesPerLiter">The number of moles per liter.</param>
let molesPerLiter (numMolesPerLiter: float) : Molarity =
    molesPerCubicMeter (numMolesPerLiter * oneMolePerLiter)

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of moles per liter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inMolesPerLiter (molarity: Molarity) : float =
    inMolesPerCubicMeter molarity / oneMolePerLiter


/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of decimoles per liter.</summary>
/// <param name="numDecimolesPerLiter">The number of decimoles per liter.</param>
let decimolesPerLiter (numDecimolesPerLiter: float) : Molarity =
    molesPerCubicMeter (numDecimolesPerLiter * oneDecimolePerLiter)

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of decimoles per liter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inDecimolesPerLiter (molarity: Molarity) : float =
    inMolesPerCubicMeter molarity
    / oneDecimolePerLiter

/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of centimoles per liter.</summary>
/// <param name="numCentimolesPerLiter">The number of centimoles per liter.</param>
let centimolesPerLiter (numCentimolesPerLiter: float) : Molarity =
    decimolesPerLiter (10. * numCentimolesPerLiter)

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of centimoles per liter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inCentimolesPerLiter (molarity: Molarity) : float = inDecimolesPerLiter molarity / 10.

/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of millimoles per liter.</summary>
/// <param name="numMillimolesPerLiter">The number of millimoles per liter.</param>
let millimolesPerLiter (numMillimolesPerLiter: float) : Molarity =
    decimolesPerLiter (100. * numMillimolesPerLiter)

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of millimoles per liter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inMillimolesPerLiter (molarity: Molarity) : float = inDecimolesPerLiter molarity / 100.

/// <category>Conversions</category>
/// <summary>Construct a molarity from a number of micromoles per liter.</summary>
/// <param name="numMicromolesPerLiter">The number of micromoles per liter.</param>
let micromolesPerLiter (numMicromolesPerLiter: float) : Molarity =
    decimolesPerLiter (1000. * numMicromolesPerLiter)

/// <category>Conversions</category>
/// <summary>Convert a molarity to a number of micromoles per liter.</summary>
/// <param name="molarity">The molarity to convert.</param>
let inMicromolesPerLiter (molarity: Molarity) : float = inDecimolesPerLiter molarity / 1000.
