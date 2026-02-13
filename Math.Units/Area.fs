/// <category>Module: Unit System</category>
/// <summary>
/// An <c>Area</c> represents an area in square meters, square feet, acres, hectares
/// etc. It is stored as a number of square meters.
/// Note that you can construct an <c>Area</c> value directly using the functions in this
/// module, but it also works to call <c>Quantity.squared</c> on a
/// <c>Length</c> or <c>Quantity.times</c> on a pair of <c>Length</c>s.
/// </summary>
///
/// <example>
/// The following are all equivalent:
/// <code>
/// Area.squareFeet 100
/// Quantity.squared (Length.feet 10)
/// Length.feet 25 |&gt; Quantity.times (Length.feet 4)
/// </code>
/// </example>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Area

/// <category>Conversions</category>
/// <summary>Construct an area from a number of square meters.</summary>
/// <param name="numSquareMeters">The number of square meters.</param>
let squareMeters (numSquareMeters: float) : Area = Quantity numSquareMeters


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square meters.</summary>
/// <param name="area">The area to convert.</param>
let inSquareMeters (area: Area) : float = area.Value


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square millimeters.</summary>
/// <param name="numSquareMillimeters">The number of square millimeters.</param>
let squareMillimeters (numSquareMillimeters: float) : Area =
    squareMeters (1.0e-6 * numSquareMillimeters)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square millimeters.</summary>
/// <param name="area">The area to convert.</param>
let inSquareMillimeters (area: Area) : float = 1.0e6 * inSquareMeters area


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square inches.</summary>
/// <param name="numSquareInches">The number of square inches.</param>
let squareInches (numSquareInches: float) : Area =
    squareMeters (Constants.squareInch * numSquareInches)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square inches.</summary>
/// <param name="area">The area to convert.</param>
let inSquareInches (area: Area) : float =
    inSquareMeters area / Constants.squareInch


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square centimeters.</summary>
/// <param name="numSquareCentimeters">The number of square centimeters.</param>
let squareCentimeters (numSquareCentimeters: float) : Area =
    squareMeters (1.0e-4 * numSquareCentimeters)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square centimeters.</summary>
/// <param name="area">The area to convert.</param>
let inSquareCentimeters (area: Area) : float = 1.0e4 * inSquareMeters area


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square feet.</summary>
/// <param name="numSquareFeet">The number of square feet.</param>
let squareFeet (numSquareFeet: float) : Area =
    squareMeters (Constants.squareFoot * numSquareFeet)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square feet.</summary>
/// <param name="area">The area to convert.</param>
let inSquareFeet (area: Area) : float =
    inSquareMeters area / Constants.squareFoot


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square yards.</summary>
/// <param name="numSquareYards">The number of square yards.</param>
let squareYards (numSquareYards: float) : Area =
    squareMeters (Constants.squareYard * numSquareYards)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square yards.</summary>
/// <param name="area">The area to convert.</param>
let inSquareYards (area: Area) : float =
    inSquareMeters area / Constants.squareYard


/// <category>Conversions</category>
/// <summary>Construct an area from a number of hectares.</summary>
/// <param name="numHectares">The number of hectares.</param>
let hectares (numHectares: float) : Area = squareMeters (1.0e4 * numHectares)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of hectares.</summary>
/// <param name="area">The area to convert.</param>
let inHectares (area: Area) : float = 1.0e-4 * inSquareMeters area


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square kilometers.</summary>
/// <param name="numSquareKilometers">The number of square kilometers.</param>
let squareKilometers (numSquareKilometers: float) : Area =
    squareMeters (1.0e6 * numSquareKilometers)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square kilometers.</summary>
/// <param name="area">The area to convert.</param>
let inSquareKilometers (area: Area) : float = 1.0e-6 * inSquareMeters area


/// <category>Conversions</category>
/// <summary>Construct an area from a number of acres.</summary>
/// <param name="numAcres">The number of acres.</param>
let acres (numAcres: float) : Area =
    squareMeters (Constants.acre * numAcres)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of acres.</summary>
/// <param name="area">The area to convert.</param>
let inAcres (area: Area) : float = inSquareMeters area / Constants.acre


/// <category>Conversions</category>
/// <summary>Construct an area from a number of square miles.</summary>
/// <param name="numSquareMiles">The number of square miles.</param>
let squareMiles (numSquareMiles: float) : Area =
    squareMeters (Constants.squareMile * numSquareMiles)


/// <category>Conversions</category>
/// <summary>Convert an area to a number of square miles.</summary>
/// <param name="area">The area to convert.</param>
let inSquareMiles (area: Area) : float =
    inSquareMeters area / Constants.squareMile


// ---- Constants --------------------------------------------------------------

/// <category>Constants</category>
/// One square meter.
let squareMeter: Area = squareMeters 1.

/// <category>Constants</category>
/// One square millimeter.
let squareMillimeter: Area =
    squareMillimeters 1.

/// <category>Constants</category>
/// One square centimeter.
let squareCentimeter: Area =
    squareCentimeters 1.

/// <category>Constants</category>
/// One hectare.
let hectare: Area = hectares 1.

/// <category>Constants</category>
/// One square kilometer.
let squareKilometer: Area =
    squareKilometers 1.

/// <category>Constants</category>
/// One square inch.
let squareInch: Area = squareInches 1.

/// <category>Constants</category>
/// One square foot.
let squareFoot: Area = squareFeet 1.

/// <category>Constants</category>
/// One square yard.
let squareYard: Area = squareYards 1.

/// <category>Constants</category>
/// One acre.
let acre: Area = acres 1.

/// <category>Constants</category>
/// One square mile.
let squareMile: Area = squareMiles 1.
