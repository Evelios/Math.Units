/// <category>Other</category>
/// <summary>
/// All conversion factors sourced from 
/// <a href="https://www.nist.gov/pml/weights-and-measures/publications/nist-handbooks/handbook-44">National Institute of Standards and Technology</a>
/// unless otherwise specified.
/// </summary>
module Math.Units.Constants

open System


// ---- Units of length (in Meters) --------------------------------------------

// ---- Metric ----

/// <summary>One meter.</summary>
[<Literal>]
let meter = 1.

/// <summary>One angstrom.</summary>
let angstrom = 1.e-10 * meter
/// <summary>One nanometer.</summary>
let nanometer = 1.e-9 * meter
/// <summary>One micron.</summary>
let micron = 1.e-6 * meter
/// <summary>One millimeter.</summary>
let millimeter = 1.e-3 * meter
/// <summary>One centimeter.</summary>
let centimeter = 1.e-2 * meter
/// <summary>One kilometer.</summary>
let kilometer = 1.e3 * meter


// ---- Imperial ----

/// <summary>One inch.</summary>
let inch = 0.0254 * meter
/// <summary>One foot.</summary>
let foot = 12. * inch
/// <summary>One yard.</summary>
let yard = 3. * foot
/// <summary>One thou.</summary>
let thou = 1.e-3 * inch
/// <summary>One mile.</summary>
let mile = 5280. * foot


// ---- Astronomical ----

/// <summary>One astronomical unit.</summary>
let astronomicalUnit = 149597870700. * meter
/// <summary>One light year.</summary>
let lightYear = 9460730472580800. * meter

/// <summary>One parsec.</summary>
let parsec =
    (648000. / Math.PI) * astronomicalUnit


// ---- Digital Conversions ----

/// <summary>One CSS pixel.</summary>
let cssPixel = inch / 96.
/// <summary>One point.</summary>
let point = inch / 72.
/// <summary>One pica.</summary>
let pica = inch / 6.



// ---- Units of Area (in Square Meters) ---------------------------------------

/// <summary>One square inch.</summary>
let squareInch: float = inch * inch
/// <summary>One square foot.</summary>
let squareFoot: float = foot * foot
/// <summary>One square yard.</summary>
let squareYard: float = yard * yard
/// <summary>One square mile.</summary>
let squareMile: float = mile * mile
/// <summary>One acre.</summary>
let acre: float = 43560. * squareFoot


// ---- Units of Volume (in Cubic Meters) --------------------------------------

/// <summary>One cubic meter.</summary>
let cubicMeter: float =
    meter * meter * meter

/// <summary>One liter.</summary>
let liter: float = 0.001 * cubicMeter

/// <summary>
/// Sourced from <a href="https://www.nist.gov/pml/weights-and-measures/publications/nist-handbooks/handbook-44">UK Weights and Measures Act</a>. One imperial gallon is equal to
/// 4.54609 cubic decimeters (formerly defined as the volume of one kilogram
/// of pure water under standard conditions, now equal to 1 liter).
/// </summary>
let imperialGallon: float = 4.54609 * liter

/// <summary>One imperial quart.</summary>
let imperialQuart: float =
    imperialGallon / 4.

/// <summary>One imperial pint.</summary>
let imperialPint: float = imperialQuart / 2.

/// <summary>One imperial fluid ounce.</summary>
let imperialFluidOunce: float =
    imperialPint / 20.

/// <summary>One cubic inch.</summary>
let cubicInch: float = inch * inch * inch
/// <summary>One cubic foot.</summary>
let cubicFoot: float = foot * foot * foot
/// <summary>One cubic yard.</summary>
let cubicYard: float = yard * yard * yard
/// <summary>One US liquid gallon.</summary>
let usLiquidGallon: float = 231. * cubicInch

/// <summary>One US liquid quart.</summary>
let usLiquidQuart: float =
    usLiquidGallon / 4.

/// <summary>One US liquid pint.</summary>
let usLiquidPint: float = usLiquidQuart / 2.
/// <summary>One US fluid ounce.</summary>
let usFluidOunce: float = usLiquidPint / 16.
/// <summary>One bushel.</summary>
let bushel: float = 2150.42 * cubicInch
/// <summary>One peck.</summary>
let peck: float = bushel / 4.
/// <summary>One US dry gallon.</summary>
let usDryGallon: float = peck / 2.

/// <summary>One US dry quart.</summary>
let usDryQuart: float =
    67.200625 * cubicInch

/// <summary>One US dry pint.</summary>
let usDryPint: float = usDryQuart / 2.


// ---- Units of Mass (in Kilograms) -------------------------------------------

/// <summary>One kilogram.</summary>
[<Literal>]
let kilogram: float = 1.

/// <summary>One pound.</summary>
let pound: float = 0.45359237 * kilogram
/// <summary>One long ton.</summary>
let longTon: float = 2240. * pound
/// <summary>One short ton.</summary>
let shortTon: float = 2000. * pound
/// <summary>One ounce.</summary>
let ounce: float = pound / 16.


/// ---- Units of Duration (in Seconds) ----------------------------------------

/// <summary>One second.</summary>
[<Literal>]
let second: float = 1.

/// <summary>One minute.</summary>
let minute: float = 60. * second
/// <summary>One hour.</summary>
let hour: float = 60. * minute
/// <summary>One day.</summary>
let day: float = 24. * hour
/// <summary>One week.</summary>
let week: float = 7. * day
/// <summary>One julian year.</summary>
let julianYear: float = 365.25 * day


/// ---- Units of substance amount (in Moles) ----------------------------------

/// <summary>One mole.</summary>
let mole: float = 1.


// ---- Units of Acceleration (in Meters per Second Squared) -------------------

/// <summary>Standard acceleration due to gravity (gee).</summary>
let gee: float =
    9.80665 * meter / (second * second)


// ---- Units of Force (in Newtons) --------------------------------------------

/// <summary>One newton.</summary>
let newton: float =
    kilogram * meter / (second * second)

/// <summary>One pound-force.</summary>
let poundForce: float =
    4.4482216152605 * newton


// ---- Units of Power (in Watts) ----------------------------------------------

/// <summary>One watt.</summary>
let watt: float = newton * meter / second

/// <summary>One electrical horsepower.</summary>
let electricalHorsepower: float =
    746. * watt

/// <summary>One mechanical horsepower.</summary>
let mechanicalHorsepower: float =
    33000. * foot * poundForce / minute

/// <summary>One metric horsepower.</summary>
let metricHorsepower: float =
    75. * kilogram * gee * meter / second


// ---- Units of Pressure (in Pascals) -----------------------------------------

/// <summary>One pascal.</summary>
let pascal: float = newton / (meter * meter)
/// <summary>One standard atmosphere.</summary>
let atmosphere: float = 101325. * pascal


// ---- Angles -----------------------------------------------------------------
    
/// <summary>Conversion factor from radians to degrees.</summary>
let radiansToDegrees: float =
    180. / Math.PI

/// <summary>Conversion factor from degrees to radians.</summary>
let degreesToRadians: float =
    Math.PI / 180.
