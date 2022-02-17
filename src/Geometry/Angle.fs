[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Angle

open System

(* Conversions *)

let pi = Radians(Math.PI)

let twoPi = Radians(2. * Math.PI)

let piOverTwo = Radians(Math.PI / 2.)

let radiansToDegrees : float = 180.0 / Math.PI

let degreesToRadians : float = Math.PI / 180.0


(* Builders *)

let inRadians = Radians

let inDegrees degrees = degrees * degreesToRadians |> Radians


(* Accessors *)

let degrees (Radians angle: Angle) : float =
    match angle with
    | radians -> radians * radiansToDegrees

let radians (Radians angle: Angle) : float = angle

(* Trig *)
let sin (Radians r) = sin r

let cos (Radians r) = cos r

let tan (Radians r) = tan r
