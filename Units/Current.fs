[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Current

/// Construct a current from a number of amperes.
let amperes (numAmperes: float) : Current = Quantity numAmperes

/// Convert a current to a number of amperes.
///    Charge.coulombs 10
///        |> Quantity.per (Duration.seconds 2)
///        |> Current.inAmperes
///    --> 5
let inAmperes (numAmperes: Current) : float = numAmperes.Value

/// Construct a current from a number of milliamperes.
///    Current.milliamperes 500
///    --> Current.amperes 0.5
let milliamperes (numMilliamperes: float) : Current = amperes (numMilliamperes * 1.0e-3)

/// Convert a current to number of milliamperes.
///    Current.amperes 2 |> Current.inMilliamperes
///    --> 2000
let inMilliamperes (current: Current) : float = inAmperes current / 1.0e-3
