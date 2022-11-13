/// <summary>
/// A <c>Volume</c> represents a volume in cubic meters, cubic feet, liters, US
/// liquid gallons, imperial fluid ounces etc. It is stored as a number of cubic
/// meters.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Volume

/// Construct a volume from a number of cubic meters.
let cubicMeters (numCubicMeters: float) : Volume = Quantity numCubicMeters

/// Convert a volume to a number of cubic meters.
let inCubicMeters (numCubicMeters: Volume) : float = numCubicMeters.Value

/// Construct a volume from a number of cubic inches.
let cubicInches (numCubicInches: float) : Volume =
    cubicMeters (Constants.cubicInch * numCubicInches)

/// Convert a volume to a number of cubic inches.
let inCubicInches (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicInch

/// Construct a volume from a number of cubic feet.
let cubicFeet (numCubicFeet: float) : Volume =
    cubicMeters (Constants.cubicFoot * numCubicFeet)

/// Convert a volume to a number of cubic feet.
let inCubicFeet (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicFoot

/// Construct a volume from a number of cubic yards.
let cubicYards (numCubicYards: float) : Volume =
    cubicMeters (Constants.cubicYard * numCubicYards)

/// Convert a volume to a number of cubic yards.
let inCubicYards (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicYard

/// Construct a volume from a number of milliliters.
let milliliters (numMilliliters: float) : Volume = cubicMeters (1.0e-6 * numMilliliters)

/// Convert a volume to a number of milliliters.
let inMilliliters (volume: Volume) : float = 1.0e6 * inCubicMeters volume

/// Construct a volume from a number of cubic centimeters.
/// Alias for <c>milliliters</c>.
let cubicCentimeters (numCubicCentimeters: float) : Volume = milliliters numCubicCentimeters

/// Convert a volume to a number of cubic centimeters.
/// Alias for <c>inMilliliters</c>.
let inCubicCentimeters (volume: Volume) : float = inMilliliters volume

/// Construct a volume from a number of liters.
let liters (numLiters: float) : Volume = cubicMeters (0.001 * numLiters)

/// Convert a volume to a number of liters.
let inLiters (volume: Volume) : float = 1000. * inCubicMeters volume

/// Construct a volume from a number of U.S. liquid gallon.
let usLiquidGallons (numUsLiquidGallons: float) : Volume =
    cubicMeters (numUsLiquidGallons * Constants.usLiquidGallon)

/// Convert a volume to a number of U.S. liquid gallons.
let inUsLiquidGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidGallon

/// Construct a volume from a number of U.S. dry gallons.
let usDryGallons (numUsDryGallons: float) : Volume =
    cubicMeters (numUsDryGallons * Constants.usDryGallon)

/// Convert a volume to a number of U.S. dry gallons.
let inUsDryGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryGallon

/// Construct a volume from a number of imperial gallons.
let imperialGallons (numImperialGallons: float) : Volume =
    cubicMeters (numImperialGallons * Constants.imperialGallon)

/// Convert a volume to a number of imperial gallons.
let inImperialGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialGallon

/// Construct a volume from a number of U.S. liquid quarts.
let usLiquidQuarts (numUsLiquidQuarts: float) : Volume =
    cubicMeters (numUsLiquidQuarts * Constants.usLiquidQuart)

/// Convert a volume to a number of U.S. liquid quarts.
let inUsLiquidQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidQuart

/// Construct a volume from a number of U.S. dry quarts.
let usDryQuarts (numUsDryQuarts: float) : Volume =
    cubicMeters (numUsDryQuarts * Constants.usDryQuart)

/// Convert a volume to a number of U.S. dry quarts.
let inUsDryQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryQuart

/// Construct a volume from a number of imperial quarts.
let imperialQuarts (numImperialQuarts: float) : Volume =
    cubicMeters (numImperialQuarts * Constants.imperialQuart)

/// Convert a volume to a number of imperial quarts.
let inImperialQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialQuart

/// Construct a volume from a number of U.S. liquid pints.
let usLiquidPints (numUsLiquidPints: float) : Volume =
    cubicMeters (numUsLiquidPints * Constants.usLiquidPint)

/// Convert a volume to a number of U.S. liquid pints.
let inUsLiquidPints (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidPint

/// Construct a volume from a number of U.S. dry pints.
let usDryPints (numUsDryPints: float) : Volume =
    cubicMeters (numUsDryPints * Constants.usDryPint)

/// Convert a volume to a number of U.S. dry pints.
let inUsDryPints (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryPint

/// Construct a volume from a number of imperial pints.
let imperialPints (numImperialPints: float) : Volume =
    cubicMeters (numImperialPints * Constants.imperialPint)

/// Convert a volume to a number of imperial pints.
let inImperialPints (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialPint

/// Construct a volume from a number of U.S. fluid ounces.
let usFluidOunces (numUsFluidOunces: float) : Volume =
    cubicMeters (numUsFluidOunces * Constants.usFluidOunce)

/// Convert a volume to a number of U.S. fluid ounces.
let inUsFluidOunces (volume: Volume) : float =
    inCubicMeters volume / Constants.usFluidOunce

/// Construct a volume from a number of imperial fluid ounces.
let imperialFluidOunces (numImperialFluidOunces: float) : Volume =
    cubicMeters (
        numImperialFluidOunces
        * Constants.imperialFluidOunce
    )

/// Convert a volume to a number of imperial fluid ounces.
let inImperialFluidOunces (volume: Volume) : float =
    inCubicMeters volume
    / Constants.imperialFluidOunce

// ---- Constants --------------------------------------------------------------

let cubicMeter: Volume = cubicMeters 1
let milliliter: Volume = milliliters 1
let cubicCentimeter: Volume = milliliters 1
let liter: Volume = liters 1
let cubicInch: Volume = cubicInches 1
let cubicFoot: Volume = cubicFeet 1
let cubicYard: Volume = cubicYards 1

let usLiquidGallon: Volume =
    usLiquidGallons 1

let usDryGallon: Volume = usDryGallons 1

let imperialGallon: Volume =
    imperialGallons 1

let usLiquidQuart: Volume = usLiquidQuarts 1
let usDryQuart: Volume = usDryQuarts 1
let imperialQuart: Volume = imperialQuarts 1
let usLiquidPint: Volume = usLiquidPints 1
let usDryPint: Volume = usDryPints 1
let imperialPint: Volume = imperialPints 1
let usFluidOunce: Volume = usFluidOunces 1

let imperialFluidOunce: Volume =
    imperialFluidOunces 1
