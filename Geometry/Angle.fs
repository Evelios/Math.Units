[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Angle

open System

// ---- Constants ----

let zero = Angle.create 0.

let pi = Angle.create Math.PI

let twoPi = Angle.create 2. * Math.PI

let piOverTwo = Angle.create Math.PI / 2.

/// Alias for `piOverTwo`
let halfPi = piOverTwo

let radiansToDegrees: float = 180.0 / Math.PI

let degreesToRadians: float = Math.PI / 180.0


// ---- Builders ----

let radians r = Angle.create r

let degrees d = d * degreesToRadians |> radians


// ---- Accessors ----

let inDegrees (angle: Angle) : float =
    match angle with
    | Angle.Radians radians -> radians * radiansToDegrees

let inRadians (Angle.Radians r: Angle) : float = r


// ---- Trig ----

let sin (r: Angle) = sin (inRadians r)
let cos (r: Angle) = cos (inRadians r)
let tan (r: Angle) = tan (inRadians r)
let asin (x: float): Angle = radians (asin x)
let acos (x: float): Angle = radians (acos x)
let atan (x: float): Angle = radians (atan x)
