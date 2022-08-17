/// A `Resistance` value represents an electrical resistance in ohms.
/// Note that since `Ohms` is defined as `Rate Volts Amperes` (voltage per unit
/// current), you can construct a `Resistance` value using `Quantity.per`:
///     resistance =
///         voltage |> Quantity.per current
/// You can also do rate-related calculations with `Resistance` values to compute
/// `Voltage` or `Current`:
///     voltage =
///         current |> Quantity.at resistance
///     current =
///         voltage |> Quantity.at_ resistance
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Resistance

/// Construct a resistance from a number of ohms.
let ohms (numOhms: float) : Resistance = Quantity numOhms

/// Convert a resistance to a number of ohms.
let inOhms (numOhms: Resistance) : float = numOhms.Value
