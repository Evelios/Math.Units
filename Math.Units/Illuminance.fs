[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Illuminance

/// Construct an illuminance value from a number of lux. One lux is equal to one
/// lumen per square meter. See [here][wp-lux-illuminance] for a table of
/// illuminance values in lux for common environments.
/// [wp-lux-illuminance]: https://en.wikipedia.org/wiki/Lux#Illuminance
let lux (numLux: float) : Illuminance = Quantity numLux

/// Convert an illuminance value to a number of lux.
let inLux (numLux: Illuminance) : float = numLux.Value

/// Construct an illuminance value from a number of
/// [foot-candles][wp-foot-candles]. One foot-candle is equal to one lumen per
/// square foot.
/// [wp-foot-candles]: https://en.wikipedia.org/wiki/Foot-candle
let footCandles (numFootCandles: float) : Illuminance =
    LuminousFlux.lumens numFootCandles
    |> Quantity.per (Area.squareFeet 1)

/// Convert an illuminance value to a number of foot-candles.
let inFootCandles (illuminance: Illuminance) : float =
    Area.squareFeet 1
    |> Quantity.at illuminance
    |> LuminousFlux.inLumens
