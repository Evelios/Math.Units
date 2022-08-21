[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Angle

open System
open Units


// ---- Builders / Accessors  ----

let radians (r: float) : Angle = Angle r
let inRadians (r: Angle) : float = r.Value

let pi: Angle = radians Math.PI
let twoPi: Angle = radians 2. * Math.PI
let piOverTwo: Angle = radians Math.PI / 2.
/// Alias for `piOverTwo`
let halfPi: Angle = piOverTwo

let degrees (d: float) : Angle =
    d * Constants.degreesToRadians |> radians

let inDegrees (angle: Angle) : float =
    angle.Value * Constants.radiansToDegrees

let turns (numTurns: float) : Angle = twoPi * numTurns
let inTurns (angle: Angle) : float = inRadians (angle / twoPi.Value)

let minutes (numMinutes: float) : Angle = degrees (numMinutes / 60.)
let inMinutes (angle: Angle) : float = 60. * inDegrees angle

let seconds (numMinutes: float) : Angle = degrees (numMinutes / 3600.)
let inSeconds (angle: Angle) : float = 3600. * inDegrees angle


// ---- Modifiers ----

/// Convert an arbitrary angle to the equivalent angle in the range -180 to 180
/// degrees (-π to π radians), by adding or subtracting some multiple of 360
/// degrees (2π radians) if necessary.
let normalize (r: Angle) : Angle =
    let turns = (r / twoPi.Value)
    let angleRadians = r - twoPi.Value * turns

    if (angleRadians.Value > Math.PI) then
        angleRadians - twoPi
    else if (angleRadians.Value < -Math.PI) then
        twoPi + angleRadians
    else
        angleRadians


// ---- Trig ----

let sin (r: Angle) : float = sin r.Value
let cos (r: Angle) : float = cos r.Value
let tan (r: Angle) : float = tan r.Value
let asin (x: float) : Angle = radians (asin x)
let acos (x: float) : Angle = radians (acos x)
let atan (x: float) : Angle = radians (atan x)


// ---- Constants ----

let radian: Angle = radians 1.
let degree: Angle = degrees 1.
let turn: Angle = turns 1.
let minute: Angle = minutes 1.
let second: Angle = seconds 1.
