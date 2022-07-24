[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Percent


// ---- Builders ----

/// Create a percentage from a ratio of numbers, a number in the range 0 to 1.
let ratio (p: float) : Percent = Percent p

/// Create a percentage from the natural range of percentages, from 0 to 100
let natural (p: float) : Percent = Percent(p * 100.)


// ---- Accessors

/// Get a percentage from a ratio of numbers, a number in the range 0 to 1.
let asRatio (p: Percent) : float = p.Value

/// Get a percentage within  the natural range of percentages, from 0 to 100
let asNatural (p: Percent) : float = p.Value * 100.
