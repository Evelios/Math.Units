[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Length

// ---- Generic ----

let zero<'Units> : Length = Length 0.

let meters m : Length = Length m
let inMeters (l: Length) : float = l.Value

let private unit constant num = meters (constant * num)
let private inUnit constant num = inMeters num / constant


// ---- Metric ----

let angstroms (l: float) : Length = unit Constants.angstrom l
let inAngstroms (l: Length) : float = inUnit Constants.angstrom l

let nanometers (l: float) : Length = unit Constants.nanometer l
let inNanometers (l: Length) : float = inUnit Constants.nanometer l

let microns (l: float) : Length = unit Constants.micron l
let inMicrons (l: Length) : float = inUnit Constants.micron l

let millimeters (l: float) : Length = unit Constants.millimeter l
let inMillimeters (l: Length) : float = inUnit Constants.millimeter l

let centimeters (l: float) : Length = unit Constants.centimeter l
let inCentimeters (l: Length) : float = inUnit Constants.centimeter l

let kilometers (l: float) : Length = unit Constants.kilometer l
let inKilometers (l: Length) : float = inUnit Constants.kilometer l


// ---- Imperial ----

let thou (l: float) : Length = unit Constants.thou l
let inThou (l: Length) : float = inUnit Constants.thou l

let inches (l: float) : Length = unit Constants.inch l
let inInches (l: Length) : float = inUnit Constants.inch l

let feet (l: float) : Length = unit Constants.foot l
let inFeet (l: Length) : float = inUnit Constants.foot l

let yards (l: float) : Length = unit Constants.yard l
let inYards (l: Length) : float = inUnit Constants.yard l

let miles (l: float) : Length = unit Constants.mile l
let inMiles (l: Length) : float = inUnit Constants.mile l


// ---- Astronomical ----

let astronomicalUnits (l: float) : Length = unit Constants.astronomicalUnit l
let inAstronomicalUnits (l: Length) : float = inUnit Constants.astronomicalUnit l

let parsecs (l: float) : Length = unit Constants.parsec l
let inParsecs (l: Length) : float = inUnit Constants.parsec l

let lightYears (l: float) : Length = unit Constants.lightYear l
let inLightYears (l: Length) : float = inUnit Constants.lightYear l


// ---- Digital ----

let cssPixels (l: float) : Length = unit Constants.cssPixel l
let inCssPixels (l: Length) : float = inUnit Constants.cssPixel l

let points (l: float) : Length = unit Constants.point l
let inPoints (l: Length) : float = inUnit Constants.point l

let picas (l: float) : Length = unit Constants.pica l
let inPicas (l: Length) : float = inUnit Constants.pica l


// ---- Constants ----

let nanometer = nanometers 1.
let micron = microns 1.
let millimeter = millimeters 1.
let centimeter = centimeters 1.
let kilometer = kilometers 1.
let oneThou = thou 1.
let inch = inches 1.
let foot = feet 1.
let yard = yards 1.
let mile = miles 1.
let astronomicalUnit = astronomicalUnits 1.
let parsec = parsecs 1.
let lightYear = lightYears 1.
let cssPixel = cssPixels 1.
let point = points 1.
let pica = picas 1.


// ---- Math ----

let apply f (l: Length) : Length = Length(f l)

let midpoint (a: Length) (b: Length) : Length = (a + b) / 2.
