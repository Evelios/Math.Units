/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Volume</c> represents a volume in cubic meters, cubic feet, liters, US
/// liquid gallons, imperial fluid ounces etc. It is stored as a number of cubic
/// meters.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Volume

// ---- Metric -----------------------------------------------------------------

/// <category index="1">Metric</category>
/// <summary>Construct a volume from a number of cubic meters.</summary>
/// <param name="numCubicMeters">The number of cubic meters.</param>
let cubicMeters (numCubicMeters: float) : Volume = Quantity numCubicMeters

/// <category>Metric</category>
/// <summary>Convert a volume to a number of cubic meters.</summary>
/// <param name="volume">The volume value to convert.</param>
let inCubicMeters (volume: Volume) : float = volume.Value

/// <category>Metric</category>
/// <summary>Construct a volume from a number of milliliters.</summary>
/// <param name="numMilliliters">The number of milliliters.</param>
let milliliters (numMilliliters: float) : Volume = cubicMeters (1.0e-6 * numMilliliters)

/// <category>Metric</category>
/// <summary>Convert a volume to a number of milliliters.</summary>
/// <param name="volume">The volume value to convert.</param>
let inMilliliters (volume: Volume) : float = 1.0e6 * inCubicMeters volume

/// <category>Metric</category>
/// <summary>Construct a volume from a number of cubic centimeters.</summary>
/// <param name="numCubicCentimeters">The number of cubic centimeters.</param>
let cubicCentimeters (numCubicCentimeters: float) : Volume = milliliters numCubicCentimeters

/// <category>Metric</category>
/// <summary>Convert a volume to a number of cubic centimeters.</summary>
/// <param name="volume">The volume value to convert.</param>
let inCubicCentimeters (volume: Volume) : float = inMilliliters volume

/// <category>Metric</category>
/// <summary>Construct a volume from a number of liters.</summary>
/// <param name="numLiters">The number of liters.</param>
let liters (numLiters: float) : Volume = cubicMeters (0.001 * numLiters)

/// <category>Metric</category>
/// <summary>Convert a volume to a number of liters.</summary>
/// <param name="volume">The volume value to convert.</param>
let inLiters (volume: Volume) : float = 1000. * inCubicMeters volume

// ---- Imperial ---------------------------------------------------------------

/// <category index="2">Imperial</category>
/// <summary>Construct a volume from a number of cubic inches.</summary>
/// <param name="numCubicInches">The number of cubic inches.</param>
let cubicInches (numCubicInches: float) : Volume =
    cubicMeters (Constants.cubicInch * numCubicInches)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of cubic inches.</summary>
/// <param name="volume">The volume value to convert.</param>
let inCubicInches (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicInch

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of cubic feet.</summary>
/// <param name="numCubicFeet">The number of cubic feet.</param>
let cubicFeet (numCubicFeet: float) : Volume =
    cubicMeters (Constants.cubicFoot * numCubicFeet)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of cubic feet.</summary>
/// <param name="volume">The volume value to convert.</param>
let inCubicFeet (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicFoot

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of cubic yards.</summary>
/// <param name="numCubicYards">The number of cubic yards.</param>
let cubicYards (numCubicYards: float) : Volume =
    cubicMeters (Constants.cubicYard * numCubicYards)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of cubic yards.</summary>
/// <param name="volume">The volume value to convert.</param>
let inCubicYards (volume: Volume) : float =
    inCubicMeters volume / Constants.cubicYard


/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. liquid gallon.</summary>
/// <param name="numUsLiquidGallons">The number of U.S. liquid gallons.</param>
let usLiquidGallons (numUsLiquidGallons: float) : Volume =
    cubicMeters (numUsLiquidGallons * Constants.usLiquidGallon)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. liquid gallons.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsLiquidGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidGallon

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. dry gallons.</summary>
/// <param name="numUsDryGallons">The number of U.S. dry gallons.</param>
let usDryGallons (numUsDryGallons: float) : Volume =
    cubicMeters (numUsDryGallons * Constants.usDryGallon)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. dry gallons.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsDryGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryGallon

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of imperial gallons.</summary>
/// <param name="numImperialGallons">The number of imperial gallons.</param>
let imperialGallons (numImperialGallons: float) : Volume =
    cubicMeters (numImperialGallons * Constants.imperialGallon)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of imperial gallons.</summary>
/// <param name="volume">The volume value to convert.</param>
let inImperialGallons (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialGallon

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. liquid quarts.</summary>
/// <param name="numUsLiquidQuarts">The number of U.S. liquid quarts.</param>
let usLiquidQuarts (numUsLiquidQuarts: float) : Volume =
    cubicMeters (numUsLiquidQuarts * Constants.usLiquidQuart)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. liquid quarts.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsLiquidQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidQuart

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. dry quarts.</summary>
/// <param name="numUsDryQuarts">The number of U.S. dry quarts.</param>
let usDryQuarts (numUsDryQuarts: float) : Volume =
    cubicMeters (numUsDryQuarts * Constants.usDryQuart)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. dry quarts.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsDryQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryQuart

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of imperial quarts.</summary>
/// <param name="numImperialQuarts">The number of imperial quarts.</param>
let imperialQuarts (numImperialQuarts: float) : Volume =
    cubicMeters (numImperialQuarts * Constants.imperialQuart)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of imperial quarts.</summary>
/// <param name="volume">The volume value to convert.</param>
let inImperialQuarts (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialQuart

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. liquid pints.</summary>
/// <param name="numUsLiquidPints">The number of U.S. liquid pints.</param>
let usLiquidPints (numUsLiquidPints: float) : Volume =
    cubicMeters (numUsLiquidPints * Constants.usLiquidPint)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. liquid pints.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsLiquidPints (volume: Volume) : float =
    inCubicMeters volume / Constants.usLiquidPint

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. dry pints.</summary>
/// <param name="numUsDryPints">The number of U.S. dry pints.</param>
let usDryPints (numUsDryPints: float) : Volume =
    cubicMeters (numUsDryPints * Constants.usDryPint)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. dry pints.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsDryPints (volume: Volume) : float =
    inCubicMeters volume / Constants.usDryPint

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of imperial pints.</summary>
/// <param name="numImperialPints">The number of imperial pints.</param>
let imperialPints (numImperialPints: float) : Volume =
    cubicMeters (numImperialPints * Constants.imperialPint)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of imperial pints.</summary>
/// <param name="volume">The volume value to convert.</param>
let inImperialPints (volume: Volume) : float =
    inCubicMeters volume / Constants.imperialPint

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of U.S. fluid ounces.</summary>
/// <param name="numUsFluidOunces">The number of U.S. fluid ounces.</param>
let usFluidOunces (numUsFluidOunces: float) : Volume =
    cubicMeters (numUsFluidOunces * Constants.usFluidOunce)

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of U.S. fluid ounces.</summary>
/// <param name="volume">The volume value to convert.</param>
let inUsFluidOunces (volume: Volume) : float =
    inCubicMeters volume / Constants.usFluidOunce

/// <category>Imperial</category>
/// <summary>Construct a volume from a number of imperial fluid ounces.</summary>
/// <param name="numImperialFluidOunces">The number of imperial fluid ounces.</param>
let imperialFluidOunces (numImperialFluidOunces: float) : Volume =
    cubicMeters (
        numImperialFluidOunces
        * Constants.imperialFluidOunce
    )

/// <category>Imperial</category>
/// <summary>Convert a volume to a number of imperial fluid ounces.</summary>
/// <param name="volume">The volume value to convert.</param>
let inImperialFluidOunces (volume: Volume) : float =
    inCubicMeters volume
    / Constants.imperialFluidOunce

// ---- Constants --------------------------------------------------------------

/// <category index="3">Constants</category>
/// One cubic meter.
let cubicMeter: Volume = cubicMeters 1

/// <category>Constants</category>
/// One milliliter meter.
let milliliter: Volume = milliliters 1

/// <category>Constants</category>
/// One cubic centimeter.
let cubicCentimeter: Volume = milliliters 1

/// <category>Constants</category>
/// One liter.
let liter: Volume = liters 1

/// <category>Constants</category>
/// One cubic inch.
let cubicInch: Volume = cubicInches 1

/// <category>Constants</category>
/// One cubic foot.
let cubicFoot: Volume = cubicFeet 1

/// <category>Constants</category>
/// One cubic yard.
let cubicYard: Volume = cubicYards 1

/// <category>Constants</category>
/// One US liquid gallon.
let usLiquidGallon: Volume =
    usLiquidGallons 1

/// <category>Constants</category>
/// One US dry gallon.
let usDryGallon: Volume = usDryGallons 1

/// <category>Constants</category>
/// One imperial gallon.
let imperialGallon: Volume =
    imperialGallons 1

/// <category>Constants</category>
/// One liquid quart.
let usLiquidQuart: Volume = usLiquidQuarts 1

/// <category>Constants</category>
/// One US dry quart.
let usDryQuart: Volume = usDryQuarts 1

/// <category>Constants</category>
/// One imperial quart.
let imperialQuart: Volume = imperialQuarts 1

/// <category>Constants</category>
/// One US liquid pint.
let usLiquidPint: Volume = usLiquidPints 1

/// <category>Constants</category>
/// One US dry pint.
let usDryPint: Volume = usDryPints 1

/// <category>Constants</category>
/// One imperial pint.
let imperialPint: Volume = imperialPints 1

/// <category>Constants</category>
/// One US fluid ounce.
let usFluidOunce: Volume = usFluidOunces 1

/// <category>Constants</category>
/// One imperial fluid ounce.
let imperialFluidOunce: Volume =
    imperialFluidOunces 1
