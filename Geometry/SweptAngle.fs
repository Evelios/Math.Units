[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.SweptAngle


/// Construct a counterclockwise arc with a swept angle between 0 and 180
/// degrees.
let smallPositive: SweptAngle = SmallPositive

/// Construct a clockwise arc with a swept angle between 0 and -180 degrees.
let smallNegative: SweptAngle = SmallNegative

/// Construct a counterclockwise arc with a swept angle between 180 and 360
/// degrees.
let largePositive: SweptAngle = LargePositive

/// Construct a clockwise arc with a swept angle between -180 and -360 degrees.
let largeNegative: SweptAngle = LargeNegative
