[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.LuminousFlux

/// Construct a luminous flux value from a number of lumens. See
/// [here][wp-luminous-flux-examples] and [here][wp-lumen-lighting] for the number
/// of lumens emitted by some common light sources.
/// [wp-luminous-flux-examples]: https://en.wikipedia.org/wiki/Luminous_flux#Examples
/// [wp-lumen-lighting]: https://en.wikipedia.org/wiki/Lumen_(unit)#Lighting
let lumens (numLumens: float) : LuminousFlux = Quantity numLumens


/// Convert a luminous flux value to a number of lumens.
let inLumens (numLumens: LuminousFlux) : float = numLumens.Value
