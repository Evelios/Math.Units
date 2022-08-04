/// An `Area` represents an area in square meters, square feet, acres, hectares
/// etc. It is stored as a number of square meters.
/// Note that you can construct an `Area` value directly using the functions in this
/// module, but it also works to call [`Quantity.squared`](Quantity#squared) on a
/// `Length` or [`Quantity.times`](Quantity#times) on a pair of `Length`s. The
/// following are all equivalent:
///     Area.squareFeet 100
///     Quantity.squared (Length.feet 10)
///     Length.feet 25 |> Quantity.times (Length.feet 4)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Area

/// Construct an area from a number of square meters.
let squareMeters (numSquareMeters: float) : Area = Quantity numSquareMeters


/// Convert an area to a number of square meters.
let inSquareMeters (numSquareMeters: Area) : float = numSquareMeters.Value


/// Construct an area from a number of square millimeters.
let squareMillimeters (numSquareMillimeters: float) : Area =
    squareMeters (1.0e-6 * numSquareMillimeters)


/// Convert an area to a number of square millimeters.
let inSquareMillimeters (area: Area) : float = 1.0e6 * inSquareMeters area


/// Construct an area from a number of square inches.
let squareInches (numSquareInches: float) : Area =
    squareMeters (Constants.squareInch * numSquareInches)


/// Convert an area to a number of square inches.
let inSquareInches (area: Area) : float =
    inSquareMeters area / Constants.squareInch


/// Construct an area from a number of square centimeters.
let squareCentimeters (numSquareCentimeters: float) : Area =
    squareMeters (1.0e-4 * numSquareCentimeters)


/// Convert an area to a number of square centimeters.
let inSquareCentimeters (area: Area) : float = 1.0e4 * inSquareMeters area


/// Construct an area from a number of square feet.
let squareFeet (numSquareFeet: float) : Area =
    squareMeters (Constants.squareFoot * numSquareFeet)


/// Convert an area to a number of square feet.
let inSquareFeet (area: Area) : float =
    inSquareMeters area / Constants.squareFoot


/// Construct an area from a number of square yards.
let squareYards (numSquareYards: float) : Area =
    squareMeters (Constants.squareYard * numSquareYards)


/// Convert an area to a number of square yards.
let inSquareYards (area: Area) : float =
    inSquareMeters area / Constants.squareYard


/// Construct an area from a number of hectares.
let hectares (numHectares: float) : Area = squareMeters (1.0e4 * numHectares)


/// Convert an area to a number of hectares.
let inHectares (area: Area) : float = 1.0e-4 * inSquareMeters area


/// Construct an area from a number of square kilometers.
let squareKilometers (numSquareKilometers: float) : Area =
    squareMeters (1.0e6 * numSquareKilometers)


/// Convert an area to a number of square kilometers.
let inSquareKilometers (area: Area) : float = 1.0e-6 * inSquareMeters area


/// Construct an area from a number of acres.
let acres (numAcres: float) : Area =
    squareMeters (Constants.acre * numAcres)


/// Convert an area to a number of acres.
let inAcres (area: Area) : float = inSquareMeters area / Constants.acre


/// Construct an area from a number of square miles.
let squareMiles (numSquareMiles: float) : Area =
    squareMeters (Constants.squareMile * numSquareMiles)


/// Convert an area to a number of square miles.
let inSquareMiles (area: Area) : float =
    inSquareMeters area / Constants.squareMile


// ---- Constants --------------------------------------------------------------

let squareMeter: Area = squareMeters 1.

let squareMillimeter: Area =
    squareMillimeters 1.

let squareCentimeter: Area =
    squareCentimeters 1.

let hectare: Area = hectares 1.

let squareKilometer: Area =
    squareKilometers 1.

let squareInch: Area = squareInches 1.
let squareFoot: Area = squareFeet 1.
let squareYard: Area = squareYards 1.
let acre: Area = acres 1.
let squareMile: Area = squareMiles 1.
