[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Illuminance

/// <summary>
/// Construct an illuminance value from a number of lux. One lux is equal to one
/// lumen per square meter. See
/// <a href="https://en.wikipedia.org/wiki/Lux#Illuminance">here</a>
/// for a table of illuminance values in lux for common environments.
/// </summary>
let lux (numLux: float) : Illuminance = Quantity numLux

/// Convert an illuminance value to a number of lux.
let inLux (numLux: Illuminance) : float = numLux.Value

/// <summary>
/// Construct an illuminance value from a number of
/// <a href="https://en.wikipedia.org/wiki/Foot-candle">foot-candles</a>.
/// One foot-candle is equal to one lumen per
/// square foot.
/// </summary>
let footCandles (numFootCandles: float) : Illuminance =
    LuminousFlux.lumens numFootCandles
    |> Quantity.per (Area.squareFeet 1)

/// Convert an illuminance value to a number of foot-candles.
let inFootCandles (illuminance: Illuminance) : float =
    Area.squareFeet 1
    |> Quantity.at illuminance
    |> LuminousFlux.inLumens
