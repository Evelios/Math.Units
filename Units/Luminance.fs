[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Luminance

open System


/// Construct a luminance value from a number of nits. One nit is equal to one
/// [candela](LuminousIntensity) per square meter, or equivalently one
/// [lux](Illuminance) per [steradian](SolidAngle).
let nits (numNits: float) : Luminance = Quantity numNits

/// Convert a luminance value to a number of nits.
let inNits (numNits: Luminance) : float = numNits.Value

/// Construct a luminance value from a number of
/// [foot-lamberts][wp-foot-lambert].
/// [wp-foot-lambert]: https://en.wikipedia.org/wiki/Foot-lambert
let footLamberts (numFootLamberts: float) : Luminance =
    LuminousIntensity.candelas numFootLamberts
    |> Quantity.per (Area.squareFeet Math.PI)


/// Convert a luminance value to a number of foot-lamberts.
let inFootLamberts (luminance: Luminance) : float =
    Area.squareFeet Math.PI
    |> Quantity.at luminance
    |> LuminousIntensity.inCandelas
