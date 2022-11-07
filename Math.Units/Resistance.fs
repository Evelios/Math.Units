/// A <c>Resistance</c> value represents an electrical resistance in ohms.
/// Note that since <c>Ohms</c> is defined as <c>Rate Volts Amperes</c> (voltage per unit
/// current), you can construct a <c>Resistance</c> value using <c>Quantity.per</c>:
///     resistance =
///         voltage |> Quantity.per current
/// You can also do rate-related calculations with <c>Resistance</c> values to compute
/// <c>Voltage</c> or <c>Current</c>:
///     voltage =
///         current |> Quantity.at resistance
///     current =
///         voltage |> Quantity.at_ resistance
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Resistance

/// Construct a resistance from a number of ohms.
let ohms (numOhms: float) : Resistance = Quantity numOhms

/// Convert a resistance to a number of ohms.
let inOhms (numOhms: Resistance) : float = numOhms.Value
