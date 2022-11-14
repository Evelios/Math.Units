[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.LuminousFlux

/// <summary>
/// Construct a luminous flux value from a number of lumens. See
/// <a href="https://en.wikipedia.org/wiki/Luminous_flux#Examples">here</a>
/// and <a href="https://en.wikipedia.org/wiki/Lumen_(unit)#Lighting">here</a>
/// for the number of lumens emitted by some common light sources.
/// </summary>
let lumens (numLumens: float) : LuminousFlux = Quantity numLumens


/// Convert a luminous flux value to a number of lumens.
let inLumens (numLumens: LuminousFlux) : float = numLumens.Value
