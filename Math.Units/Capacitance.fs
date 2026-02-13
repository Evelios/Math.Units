/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Capacitance</c> value represents an electrical capacitance in farads.
/// Note that since <c>Capacitance</c> is defined as <c>Rate&lt;Coulombs, Volts&gt;</c> (charge per
/// voltage) .
/// </summary>
/// 
/// <example>
/// <para>
/// You can construct a <c>Capacitance</c> value using <c>Quantity.per</c>:
/// </para>
/// <code>
///     let capacitance =
///         charge |&gt; Quantity.per voltage
/// </code>
/// <para>
/// You can also compute <c>Charge</c> and <c>Voltage</c> using <c>Capacitance</c>:
/// </para>
/// <code>
///     let charge =
///         voltage |&gt; Quantity.at capacitance
///     let voltage =
///         charge |&gt; Quantity.at_ capacitance
/// </code>
/// </example>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Capacitance

/// <category>Conversions</category>
/// <summary>Construct capacitance from a number of farads.</summary>
/// <param name="numFarads">The number of farads.</param>
let farads (numFarads: float) : Capacitance = Quantity numFarads

/// <category>Conversions</category>
/// <summary>Convert capacitance to a number of farads.</summary>
/// <param name="capacitance">The capacitance to convert.</param>
let inFarads (capacitance: Capacitance) : float = capacitance.Value

/// <category>Conversions</category>
/// <summary>Construct a capacitance from a number of microfarads.</summary>
/// <param name="numMicrofarads">The number of microfarads.</param>
let microfarads (numMicrofarads: float) = farads (numMicrofarads * 1.0e-6)

/// <category>Conversions</category>
/// <summary>Convert a capacitance to a number of microfarads.</summary>
/// <param name="capacitance">The capacitance to convert.</param>
let inMicrofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-6

/// <category>Conversions</category>
/// <summary>Construct a capacitance from a number of nanofarads.</summary>
/// <param name="numNanofarads">The number of nanofarads.</param>
let nanofarads (numNanofarads: float) : Capacitance = farads (numNanofarads * 1.0e-9)

/// <category>Conversions</category>
/// <summary>Convert a capacitance to a number of nanofarads.</summary>
/// <param name="capacitance">The capacitance to convert.</param>
let inNanofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-9

/// <category>Conversions</category>
/// <summary>Construct capacitance from a number of picofarads.</summary>
/// <param name="numPicofarads">The number of picofarads.</param>
let picofarads (numPicofarads: float) : Capacitance = farads (numPicofarads * 1.0e-12)

/// <category>Conversions</category>
/// <summary>Convert a capacitance to a number of picofarads.</summary>
/// <param name="capacitance">The capacitance to convert.</param>
let inPicofarads (capacitance: Capacitance) : float = inFarads capacitance / 1.0e-12
