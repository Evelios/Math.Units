/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Length</c> represents a length in meters, feet, centimeters, miles etc. It
/// is stored as a number of meters.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Length

// ---- Math ----

/// <category index="1">Modifiers</category>
/// <param name="f">The modifier function.</param>
/// <param name="l">The length to modify.</param>
let apply f (l: Length) : Length = Length(f l)

/// <category>Modifiers</category>
/// <param name="a">The first length.</param>
/// <param name="b">The second length.</param>
let midpoint (a: Length) (b: Length) : Length = (a + b) / 2.

// ---- Generic ----

/// <category index="2">Metric</category>
/// <param name="m">The number of meters.</param>
let meters m : Length = Length m
/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inMeters (l: Length) : float = l.Value

let private unit constant num = meters (constant * num)
let private inUnit constant num = inMeters num / constant


// ---- Metric ----

/// <category>Metric</category>
/// <param name="l">The number of angstroms.</param>
let angstroms (l: float) : Length = unit Constants.angstrom l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inAngstroms (l: Length) : float = inUnit Constants.angstrom l

/// <category>Metric</category>
/// <param name="l">The number of nanometers.</param>
let nanometers (l: float) : Length = unit Constants.nanometer l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inNanometers (l: Length) : float = inUnit Constants.nanometer l

/// <category>Metric</category>
/// <param name="l">The number of microns.</param>
let microns (l: float) : Length = unit Constants.micron l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inMicrons (l: Length) : float = inUnit Constants.micron l

/// <category>Metric</category>
/// <param name="l">The number of millimeters.</param>
let millimeters (l: float) : Length = unit Constants.millimeter l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inMillimeters (l: Length) : float = inUnit Constants.millimeter l

/// <category>Metric</category>
/// <param name="l">The number of centimeters.</param>
let centimeters (l: float) : Length = unit Constants.centimeter l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inCentimeters (l: Length) : float = inUnit Constants.centimeter l

/// <category>Metric</category>
/// <param name="l">The number of kilometers.</param>
let kilometers (l: float) : Length = unit Constants.kilometer l

/// <category>Metric</category>
/// <param name="l">The length to convert.</param>
let inKilometers (l: Length) : float = inUnit Constants.kilometer l


// ---- Imperial ----

/// <category index="3">Imperial</category>
/// <param name="l">The number of thou.</param>
let thou (l: float) : Length = unit Constants.thou l

/// <category>Imperial</category>
/// <param name="l">The length to convert.</param>
let inThou (l: Length) : float = inUnit Constants.thou l

/// <category>Imperial</category>
/// <param name="l">The number of inches.</param>
let inches (l: float) : Length = unit Constants.inch l

/// <category>Imperial</category>
/// <param name="l">The length to convert.</param>
let inInches (l: Length) : float = inUnit Constants.inch l

/// <category>Imperial</category>
/// <param name="l">The number of feet.</param>
let feet (l: float) : Length = unit Constants.foot l

/// <category>Imperial</category>
/// <param name="l">The length to convert.</param>
let inFeet (l: Length) : float = inUnit Constants.foot l

/// <category>Imperial</category>
/// <param name="l">The number of yards.</param>
let yards (l: float) : Length = unit Constants.yard l

/// <category>Imperial</category>
/// <param name="l">The length to convert.</param>
let inYards (l: Length) : float = inUnit Constants.yard l

/// <category>Imperial</category>
/// <param name="l">The number of miles.</param>
let miles (l: float) : Length = unit Constants.mile l

/// <category>Imperial</category>
/// <param name="l">The length to convert.</param>
let inMiles (l: Length) : float = inUnit Constants.mile l


// ---- Digital ----

/// <category index="4">Typography</category>
/// <param name="l">The number of css pixels.</param>
let cssPixels (l: float) : Length = unit Constants.cssPixel l

/// <category>Typography</category>
/// <param name="l">The length to convert.</param>
let inCssPixels (l: Length) : float = inUnit Constants.cssPixel l

/// <category>Typography</category>
/// <param name="l">The number of points.</param>
let points (l: float) : Length = unit Constants.point l

/// <category>Typography</category>
/// <param name="l">The length to convert.</param>
let inPoints (l: Length) : float = inUnit Constants.point l

/// <category>Typography</category>
/// <param name="l">The number of picas.</param>
let picas (l: float) : Length = unit Constants.pica l

/// <category>Typography</category>
/// <param name="l">The length to convert.</param>
let inPicas (l: Length) : float = inUnit Constants.pica l


// ---- Astronomical ----

/// <category index="5">Astronomical</category>
/// <param name="l">The number of astronomical units.</param>
let astronomicalUnits (l: float) : Length = unit Constants.astronomicalUnit l

/// <category>Astronomical</category>
/// <param name="l">The length to convert.</param>
let inAstronomicalUnits (l: Length) : float = inUnit Constants.astronomicalUnit l

/// <category>Astronomical</category>
/// <param name="l">The number of parsecs.</param>
let parsecs (l: float) : Length = unit Constants.parsec l

/// <category>Astronomical</category>
/// <param name="l">The length to convert.</param>
let inParsecs (l: Length) : float = inUnit Constants.parsec l

/// <category>Astronomical</category>
/// <param name="l">The number of light years.</param>
let lightYears (l: float) : Length = unit Constants.lightYear l

/// <category>Astronomical</category>
/// <param name="l">The length to convert.</param>
let inLightYears (l: Length) : float = inUnit Constants.lightYear l


// ---- Constants ----

/// <category index="6">Constants</category>
let nanometer = nanometers 1.
/// <category>Constants</category>
let micron = microns 1.
/// <category>Constants</category>
let millimeter = millimeters 1.
/// <category>Constants</category>
let centimeter = centimeters 1.
/// <category>Constants</category>
let kilometer = kilometers 1.
/// <category>Constants</category>
let oneThou = thou 1.
/// <category>Constants</category>
let inch = inches 1.
/// <category>Constants</category>
let foot = feet 1.
/// <category>Constants</category>
let yard = yards 1.
/// <category>Constants</category>
let mile = miles 1.
/// <category>Constants</category>
let astronomicalUnit = astronomicalUnits 1.
/// <category>Constants</category>
let parsec = parsecs 1.
/// <category>Constants</category>
let lightYear = lightYears 1.
/// <category>Constants</category>
let cssPixel = cssPixels 1.
/// <category>Constants</category>
let point = points 1.
/// <category>Constants</category>
let pica = picas 1.


