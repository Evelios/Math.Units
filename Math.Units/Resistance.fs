/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Resistance</c> value represents an electrical resistance in ohms.
/// </summary>
/// 
/// <note>
/// <para>
/// Since <c>Ohms</c> is defined as <c>Rate Volts Amperes</c> (voltage per unit
/// current), you can construct a <c>Resistance</c> value using <c>Quantity.per</c>:
/// </para>
/// <code>
///     let resistance =
///         voltage |&gt; Quantity.per current
/// </code>
/// <para>
/// You can also do rate-related calculations with <c>Resistance</c> values to compute
/// </para>
/// <code>
/// <c>Voltage</c> or <c>Current</c>:
///     let voltage =
///         current |&gt; Quantity.at resistance
///     let current =
///         voltage |&gt; Quantity.at_ resistance
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Resistance

/// <category>Conversions</category>
/// <summary>Construct a resistance from a number of ohms.</summary>
/// <param name="numOhms">The number of ohms.</param>
let ohms (numOhms: float) : Resistance = Quantity numOhms

/// <category>Conversions</category>
/// <summary>Convert a resistance to a number of ohms.</summary>
/// <param name="resistance">The resistance value to convert.</param>
let inOhms (resistance: Resistance) : float = resistance.Value
