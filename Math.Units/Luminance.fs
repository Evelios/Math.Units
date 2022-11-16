/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Luminance

open System

/// <summary>
/// Construct a luminance value from a number of nits. One nit is equal to one
/// <b>candela</b> per square meter, or equivalently one <b>lux</b> per <b>steradian</b>.
/// </summary>
let nits (numNits: float) : Luminance = Quantity numNits

/// Convert a luminance value to a number of nits.
let inNits (numNits: Luminance) : float = numNits.Value

/// <summary>
/// Construct a luminance value from a number of
/// <a href="https://en.wikipedia.org/wiki/Foot-lambert">foot-lamberts</a>.
/// </summary>
let footLamberts (numFootLamberts: float) : Luminance =
    LuminousIntensity.candelas numFootLamberts
    |> Quantity.per (Area.squareFeet Math.PI)


/// Convert a luminance value to a number of foot-lamberts.
let inFootLamberts (luminance: Luminance) : float =
    Area.squareFeet Math.PI
    |> Quantity.at luminance
    |> LuminousIntensity.inCandelas
