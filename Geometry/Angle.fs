[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Angle

open System

(* Constants *)

let pi = Angle.create Math.PI

let twoPi = Angle.create 2. * Math.PI

let piOverTwo = Angle.create Math.PI / 2.

/// Alias for `piOverTwo`
let halfPi = piOverTwo

let radiansToDegrees : float = 180.0 / Math.PI

let degreesToRadians : float = Math.PI / 180.0


(* Builders *)

let radians r = Angle.create r


let degrees d = d * degreesToRadians |> radians


(* Accessors *)

let inDegrees (angle: Angle) : float =
    match angle with
    | radians -> radians.value() * radiansToDegrees

let inRadians (angle: Angle) : float = angle.value()

(* Trig *)
let sin (r: Angle) = sin (inRadians r)

let cos (r: Angle) = cos (inRadians r)

let tan (r: Angle) = tan (inRadians r)