[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Percent


// ---- Builders ----

/// Create a percentage from a ratio of numbers, a number in the range 0 to 1.
let ratio (p: float) : Percent = Percent p

/// Create a percentage from the natural range of percentages, from 0 to 100
let natural (p: float) : Percent = Percent(p * 100.)


// ---- Accessors

/// Get a percentage from a ratio of numbers, a number in the range 0 to 1.
let asRatio (Percent p: Percent) : float = p

/// Get a percentage within  the natural range of percentages, from 0 to 100
let asNatural (Percent p: Percent) : float = p * 100.
