[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Length

// Generic

let zero<'Unit> : Length<'Unit> = Length<'Unit>.create 0.

// Pixels

let pixels p : Length<Pixels> = Length<Pixels>.create p

let inPixels (l: 'Unit Length) : float = Length<'Unit>.value l


// Metric

let meters m : Length<Meters> = Length<Meters>.create m

let inMeters (l: 'Unit Length) : float = Length<'Unit>.value l

// Unitless

let unitless l : Length<Unitless> = Length<Unitless>.create l

// Math

let apply f (l : Length<'Unit>) : Length<'Unit> =
    Length<'Unit>.create (f (l.value()))

let square (l: Length<'Unit>): Length<'Unit*'Unit> =
    l * l

let sqrt (l : Length<'Unit*'Unit>) : Length<'Unit> =
    Length<'Unit>.create (sqrt (l.value()))
    
let round (l : Length<'Unit>) : Length<'Unit> =
    apply roundFloat l
    
let roundTo (digits: int) (l : Length<'Unit>) : Length<'Unit> =
    apply (roundFloatTo digits) l
