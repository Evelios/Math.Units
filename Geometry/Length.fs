[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Length

open System

/// Units of length in meters
module Constant =

    // ---- Metric ----

    [<Literal>]
    let meter = 1.0

    let nanometer = 1.0e-9 * meter
    let micron = 1.0e-6 * meter
    let millimeter = 1.0e-3 * meter
    let centimeter = 1.0e-2 * meter
    let kilometer = 1.0e3 * meter


    // ---- Imperial ----

    let inch = 0.0254 * meter
    let foot = 12. * inch
    let yard = 3. * foot
    let thou = 1.0e-3 * inch
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

let zero<'Unit> : Length<'Unit> = Length<'Unit>.create 0.

let meters m : Length<Meters> = Length<Meters>.create m
let inMeters (Length.Length l: Length<'Unit>) : float = l

let private unit constant num = meters (constant * num)
let private inUnit constant num = constant * inMeters num


// ---- Metric ----

let nanometers (l: float) : Length<Meters> = unit Constant.nanometer l
let inNanometers (l: Length<Meters>) : float = inUnit Constant.nanometer l

let microns (l: float) : Length<Meters> = unit Constant.micron l
let inMicrons (l: Length<Meters>) : float = inUnit Constant.micron l

let millimeters (l: float) : Length<Meters> = unit Constant.millimeter l
let inMillimeters (l: Length<Meters>) : float = inUnit Constant.millimeter l

let centimeters (l: float) : Length<Meters> = unit Constant.centimeter l
let inCentimeters (l: Length<Meters>) : float = inUnit Constant.centimeter l

let kilometers (l: float) : Length<Meters> = unit Constant.kilometer l
let inKilometers (l: Length<Meters>) : float = inUnit Constant.kilometer l


// ---- Imperial ----

let thou (l: float) : Length<Meters> = unit Constant.thou l
let inThou (l: Length<Meters>) : float = inUnit Constant.thou l

let inches (l: float) : Length<Meters> = unit Constant.inch l
let inInches (l: Length<Meters>) : float = inUnit Constant.inch l

let feet (l: float) : Length<Meters> = unit Constant.foot l
let inFeet (l: Length<Meters>) : float = inUnit Constant.foot l

let yards (l: float) : Length<Meters> = unit Constant.yard l
let inYards (l: Length<Meters>) : float = inUnit Constant.yard l

let miles (l: float) : Length<Meters> = unit Constant.mile l
let inMiles (l: Length<Meters>) : float = inUnit Constant.mile l


// ---- Astronomical ----

let astronomicalUnits (l: float) : Length<Meters> = unit Constant.astronomicalUnit l
let inAstronomicalUnits (l: Length<Meters>) : float = inUnit Constant.astronomicalUnit l

let parsecs (l: float) : Length<Meters> = unit Constant.parsec l
let inParsecs (l: Length<Meters>) : float = inUnit Constant.parsec l

let lightYears (l: float) : Length<Meters> = unit Constant.lightYear l
let inLightYears (l: Length<Meters>) : float = inUnit Constant.lightYear l


// ---- Digital ----

let cssPixels (l: float) : Length<Meters> = unit Constant.cssPixel l
let inCssPixels (l: Length<Meters>) : float = inUnit Constant.cssPixel l

let points (l: float) : Length<Meters> = unit Constant.point l
let inPoints (l: Length<Meters>) : float = inUnit Constant.point l

let picas (l: float) : Length<Meters> = unit Constant.pica l
let inPicas (l: Length<Meters>) : float = inUnit Constant.pica l


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

let unitless l : Length<Unitless> = Length<Unitless>.create l


// ---- Math ----

let apply f (Length.Length l: Length<'Unit>) : Length<'Unit> = Length<'Unit>.create (f l)

let square (l: Length<'Unit>) : Length<'Unit * 'Unit> = l * l

let sqrt (Length.Length l: Length<'Unit * 'Unit>) : Length<'Unit> = Length<'Unit>.create (sqrt l)

let twice (Length.Length l: Length<'Unit>) : Length<'Unit> = Length<'Unit>.create (l * 2.)

let half (Length.Length l: Length<'Unit>) : Length<'Unit> = Length<'Unit>.create (l / 2.)

let round (l: Length<'Unit>) : Length<'Unit> = apply roundFloat l

let roundTo (digits: int) (l: Length<'Unit>) : Length<'Unit> = apply (roundFloatTo digits) l

let abs (Length.Length l: Length<'Unit>) : Length<'Unit> = Length<'Unit>.create (abs l)

let min (Length.Length l1: Length<'Unit>) (Length.Length l2: Length<'Unit>) : Length<'Unit> =
    Length<'Unit>.create (min l1 l2)

let max (Length.Length l1: Length<'Unit>) (Length.Length l2: Length<'Unit>) : Length<'Unit> =
    Length<'Unit>.create (max l1 l2)

let sum (lengths: Length<'Unit> seq) : Length<'Unit> = Seq.fold (+) zero lengths

// ---- Unsafe ----

let unpack (Length.Length l: Length<'Unit>) : float = l
