/// <category>Module: Unit System</category>
/// <summary>
/// A <c>LuminousFlux</c> value represents the total amount of light emitted by a
/// light source. You can think of it as roughly "photons per second", although
/// <a href="https://en.wikipedia.org/wiki/Luminous_flux">it's a bit more complicated than that</a>.
/// Luminous flux is stored in <a href="https://en.wikipedia.org/wiki/Lumen_(unit)">lumens</a>. It's often used to describe the
/// total output of a light bulb; for example, a 50 watt incandescent bulb and a 6
/// watt LED bulb might each have an output of 400 lumens.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.LuminousFlux

/// <category>Conversions</category>
/// <summary>
/// Construct a luminous flux value from a number of lumens. See
/// <a href="https://en.wikipedia.org/wiki/Luminous_flux#Examples">here</a>
/// and <a href="https://en.wikipedia.org/wiki/Lumen_(unit)#Lighting">here</a>
/// for the number of lumens emitted by some common light sources.
/// </summary>
/// <param name="numLumens">The number of lumens.</param>
let lumens (numLumens: float) : LuminousFlux = Quantity numLumens


/// <category>Conversions</category>
/// <summary>Convert a luminous flux value to a number of lumens.</summary>
/// <param name="luminousFlux">The luminous flux value to convert.</param>
let inLumens (luminousFlux: LuminousFlux) : float = luminousFlux.Value
