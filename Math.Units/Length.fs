/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Length</c> represents a length in meters, feet, centimeters, miles etc. It
/// is stored as a number of meters.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Length

// ---- Math ----

/// <category index="1">Modifiers</category>
let apply f (l: Length) : Length = Length(f l)

/// <category>Modifiers</category>
let midpoint (a: Length) (b: Length) : Length = (a + b) / 2.

// ---- Generic ----

/// <category index="2">Metric</category>
let meters m : Length = Length m
/// <category>Metric</category>
let inMeters (l: Length) : float = l.Value

let private unit constant num = meters (constant * num)
let private inUnit constant num = inMeters num / constant


// ---- Metric ----

/// <category>Metric</category>
let angstroms (l: float) : Length = unit Constants.angstrom l

/// <category>Metric</category>
let inAngstroms (l: Length) : float = inUnit Constants.angstrom l

/// <category>Metric</category>
let nanometers (l: float) : Length = unit Constants.nanometer l

/// <category>Metric</category>
let inNanometers (l: Length) : float = inUnit Constants.nanometer l

/// <category>Metric</category>
let microns (l: float) : Length = unit Constants.micron l

/// <category>Metric</category>
let inMicrons (l: Length) : float = inUnit Constants.micron l

/// <category>Metric</category>
let millimeters (l: float) : Length = unit Constants.millimeter l

/// <category>Metric</category>
let inMillimeters (l: Length) : float = inUnit Constants.millimeter l

/// <category>Metric</category>
let centimeters (l: float) : Length = unit Constants.centimeter l

/// <category>Metric</category>
let inCentimeters (l: Length) : float = inUnit Constants.centimeter l

/// <category>Metric</category>
let kilometers (l: float) : Length = unit Constants.kilometer l

/// <category>Metric</category>
let inKilometers (l: Length) : float = inUnit Constants.kilometer l


// ---- Imperial ----

/// <category index="3">Imperial</category>
let thou (l: float) : Length = unit Constants.thou l

/// <category>Imperial</category>
let inThou (l: Length) : float = inUnit Constants.thou l

/// <category>Imperial</category>
let inches (l: float) : Length = unit Constants.inch l

/// <category>Imperial</category>
let inInches (l: Length) : float = inUnit Constants.inch l

/// <category>Imperial</category>
let feet (l: float) : Length = unit Constants.foot l

/// <category>Imperial</category>
let inFeet (l: Length) : float = inUnit Constants.foot l

/// <category>Imperial</category>
let yards (l: float) : Length = unit Constants.yard l

/// <category>Imperial</category>
let inYards (l: Length) : float = inUnit Constants.yard l

/// <category>Imperial</category>
let miles (l: float) : Length = unit Constants.mile l

/// <category>Imperial</category>
let inMiles (l: Length) : float = inUnit Constants.mile l


// ---- Digital ----

/// <category index="4">Typography</category>
let cssPixels (l: float) : Length = unit Constants.cssPixel l

/// <category>Typography</category>
let inCssPixels (l: Length) : float = inUnit Constants.cssPixel l

/// <category>Typography</category>
let points (l: float) : Length = unit Constants.point l

/// <category>Typography</category>
let inPoints (l: Length) : float = inUnit Constants.point l

/// <category>Typography</category>
let picas (l: float) : Length = unit Constants.pica l

/// <category>Typography</category>
let inPicas (l: Length) : float = inUnit Constants.pica l


// ---- Astronomical ----

/// <category index="5">Astronomical</category>
let astronomicalUnits (l: float) : Length = unit Constants.astronomicalUnit l

/// <category>Astronomical</category>
let inAstronomicalUnits (l: Length) : float = inUnit Constants.astronomicalUnit l

/// <category>Astronomical</category>
let parsecs (l: float) : Length = unit Constants.parsec l

/// <category>Astronomical</category>
let inParsecs (l: Length) : float = inUnit Constants.parsec l

/// <category>Astronomical</category>
let lightYears (l: float) : Length = unit Constants.lightYear l

/// <category>Astronomical</category>
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


