namespace Geometry

open System
open LanguagePrimitives

[<Measure>]
type rad

[<Measure>]
type deg

[<Struct>]
type Angle =
    private
    | Radians of float<rad>

    (* Operators *)

    static member (+)(lhs: Angle, rhs: Angle) : Angle =
        match lhs, rhs with
        | Radians l, Radians r -> Radians(l + r)

    static member (-)(lhs: Angle, rhs: Angle) : Angle =
        match lhs, rhs with
        | Radians l, Radians r -> Radians(l - r)

    static member (~-)(Radians angle: Angle) : Angle = Radians -angle

    static member (*)(lhs: Angle, rhs: float) : Angle =
        match lhs with
        | Radians l -> Radians(l * rhs)

    static member (*)(lhs: float, rhs: Angle) : Angle = rhs * lhs

    static member (/)(lhs: Angle, rhs: float) : Angle =
        match lhs with
        | Radians l -> Radians(l / rhs)

    static member (/)(lhs: float, rhs: Angle) : Angle = rhs / lhs


module Angle =
    (* Conversions *)

    let pi = Radians(FloatWithMeasure Math.PI)

    let twoPi = Radians(FloatWithMeasure(2. * Math.PI))

    let piOverTwo = Radians(FloatWithMeasure(Math.PI / 2.))

    let radiansToDegrees: float<deg / rad> = FloatWithMeasure 180.0 / Math.PI

    let degreesToRadians: float<rad / deg> = FloatWithMeasure Math.PI / 180.0


    (* Builders *)

    let inRadians = Radians

    let inDegrees degrees = degrees * degreesToRadians |> Radians


    (* Accessors *)

    let degrees (Radians angle: Angle) : float<deg> =
        match angle with
        | radians -> radians * radiansToDegrees

    let radians (Radians angle: Angle) : float<rad> = angle

    (* Trig *)
    let sin (Radians r) = sin (float r)

    let cos (Radians r) = cos (float r)

    let tan (Radians r) = tan (float r)
