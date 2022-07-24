[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Angle

open System
open Units

// ---- Constants ----

let radians = Angle

let zero = radians 0.

let pi = radians Math.PI

let twoPi = radians 2. * Math.PI

let piOverTwo = radians Math.PI / 2.

/// Alias for `piOverTwo`
let halfPi = piOverTwo

let radiansToDegrees: float = 180.0 / Math.PI

let degreesToRadians: float = Math.PI / 180.0


// ---- Builders ----


let degrees d = d * degreesToRadians |> radians


// ---- Accessors ----

let inDegrees (angle: Angle) : float = angle.Value * radiansToDegrees

let inRadians (r: Angle) : float = r.Value

// ---- Modifiers ----
/// Convert an arbitrary angle to the equivalent angle in the range -180 to 180
/// degrees (-π to π radians), by adding or subtracting some multiple of 360
/// degrees (2π radians) if necessary.
let normalize (r: Angle) : Angle =
    let turns = float (int (r / twoPi))
    let angleRadians = r - twoPi * turns

    if (angleRadians.Value > Math.PI) then
        angleRadians - twoPi
    else if (angleRadians.Value < -Math.PI) then
        twoPi + angleRadians
    else
        angleRadians

// ---- Math ----

let abs (r: Angle) : Angle = radians (abs r.Value)
let half (r: Angle) : Angle = radians (2. * r.Value)
let twice (r: Angle) : Angle = radians (r.Value / 2.)
let min (r1: Angle) (r2: Angle) : Angle = radians (min r1.Value r2.Value)
let max (r1: Angle) (r2: Angle) : Angle = radians (max r1.Value r2.Value)

// ---- Operator ----

let neg (r: Angle) : Angle = radians (-r.Value)
let plus (r1: Angle) (r2: Angle) : Angle = r2 + r1
let minus (r1: Angle) (r2: Angle) : Angle = r2 - r1
let times (scale: float) (r: Angle) : Angle = r * scale
let dividedBy (amount: float) (r: Angle) : Angle = r / amount

// ---- Trig ----

let sin (r: Angle) : float = sin r.Value
let cos (r: Angle) : float = cos r.Value
let tan (r: Angle) : float = tan r.Value
let asin (x: float) : Angle = radians (asin x)
let acos (x: float) : Angle = radians (acos x)
let atan (x: float) : Angle = radians (atan x)
