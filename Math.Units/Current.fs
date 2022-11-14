[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Current

/// Construct a current from a number of amperes.
let amperes (numAmperes: float) : Current = Quantity numAmperes


/// <summary>
/// Convert a current to a number of amperes.
/// </summary>
/// 
/// <example><code>
///    Charge.coulombs 10
///        |> Quantity.per (Duration.seconds 2)
///        |> Current.inAmperes
///    --> 5
/// </code></example>
let inAmperes (numAmperes: Current) : float = numAmperes.Value

/// <summary>
/// Construct a current from a number of milliamperes.
/// </summary>
/// 
/// <example><code>
///    Current.milliamperes 500
///    --> Current.amperes 0.5
/// </code></example>
let milliamperes (numMilliamperes: float) : Current = amperes (numMilliamperes * 1.0e-3)

/// <summary>
/// Convert a current to number of milliamperes.
/// </summary>
/// 
/// <example><code>
///    Current.amperes 2 |> Current.inMilliamperes
///    --> 2000
/// </code></example>
let inMilliamperes (current: Current) : float = inAmperes current / 1.0e-3
