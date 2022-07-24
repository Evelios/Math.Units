[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Length

open System

/// Units of length in meters
module Constant =

    // ---- Metric ----

    [<Literal>]
    let meter = 1.0

    let angstrom = 1.e-10 * meter
    let nanometer = 1.e-9 * meter
    let micron = 1.e-6 * meter
    let millimeter = 1.e-3 * meter
    let centimeter = 1.e-2 * meter
    let kilometer = 1.e3 * meter


    // ---- Imperial ----

    let inch = 0.0254 * meter
    let foot = 12. * inch
    let yard = 3. * foot
    let thou = 1.e-3 * inch
    let mile = 5280. * foot


    // ---- Astronomical ----

    let astronomicalUnit = 149597870700. * meter
    let lightYear = 9460730472580800. * meter
    let parsec = (648000. / Math.PI) * astronomicalUnit


    // ---- Digital Conversions ----

    let cssPixel = inch / 96.
    let point = inch / 72.
    let pica = inch / 6.


// ---- Generic ----

let zero<'Unit> : Length = Length 0.

let meters m : Length = Length m
let inMeters (l: Length) : float = l.Value

let private unit constant num = meters (constant * num)
let private inUnit constant num = inMeters num / constant


// ---- Metric ----

let angstroms (l: float) : Length = unit Constant.angstrom l
let inAngstroms (l: Length) : float = inUnit Constant.angstrom l

let nanometers (l: float) : Length = unit Constant.nanometer l
let inNanometers (l: Length) : float = inUnit Constant.nanometer l

let microns (l: float) : Length = unit Constant.micron l
let inMicrons (l: Length) : float = inUnit Constant.micron l

let millimeters (l: float) : Length = unit Constant.millimeter l
let inMillimeters (l: Length) : float = inUnit Constant.millimeter l

let centimeters (l: float) : Length = unit Constant.centimeter l
let inCentimeters (l: Length) : float = inUnit Constant.centimeter l

let kilometers (l: float) : Length = unit Constant.kilometer l
let inKilometers (l: Length) : float = inUnit Constant.kilometer l


// ---- Imperial ----

let thou (l: float) : Length = unit Constant.thou l
let inThou (l: Length) : float = inUnit Constant.thou l

let inches (l: float) : Length = unit Constant.inch l
let inInches (l: Length) : float = inUnit Constant.inch l

let feet (l: float) : Length = unit Constant.foot l
let inFeet (l: Length) : float = inUnit Constant.foot l

let yards (l: float) : Length = unit Constant.yard l
let inYards (l: Length) : float = inUnit Constant.yard l

let miles (l: float) : Length = unit Constant.mile l
let inMiles (l: Length) : float = inUnit Constant.mile l


// ---- Astronomical ----

let astronomicalUnits (l: float) : Length = unit Constant.astronomicalUnit l
let inAstronomicalUnits (l: Length) : float = inUnit Constant.astronomicalUnit l

let parsecs (l: float) : Length = unit Constant.parsec l
let inParsecs (l: Length) : float = inUnit Constant.parsec l

let lightYears (l: float) : Length = unit Constant.lightYear l
let inLightYears (l: Length) : float = inUnit Constant.lightYear l


// ---- Digital ----

let cssPixels (l: float) : Length = unit Constant.cssPixel l
let inCssPixels (l: Length) : float = inUnit Constant.cssPixel l

let points (l: float) : Length = unit Constant.point l
let inPoints (l: Length) : float = inUnit Constant.point l

let picas (l: float) : Length = unit Constant.pica l
let inPicas (l: Length) : float = inUnit Constant.pica l


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


// ---- Unitless ----

let unitless l : Length = Length l


// ---- Math ----

let apply f (l: Length) : Length = Length (f l)

// Type conversion issue
//let midpoint (a: Length) (b: Length) : Length = (a + b) / 2.