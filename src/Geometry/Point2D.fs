[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Point2D

open FSharp.Json

(* Builders *)

let xy (x: Length<'Unit>) (y: Length<'Unit>) : Point2D<'Unit, 'Coordinates> = { X = x; Y = y }

let meters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = xy (Length.meters x) (Length.meters y)

let rTheta (r: Length<'Unit>) (theta: Angle) : Point2D<'Unit, 'Coordinates> =
    xy (r * Angle.cos theta) (r * Angle.sin theta)

let ofPolar r theta = rTheta r theta

let origin () : Point2D<'Unit, 'Coordinates> = xy Length.zero Length.zero

(* Modifiers *)

let toVector (point: Point2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = Vector2D.xy point.X point.Y

let scale (x: float) (y: float) (point: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    xy (point.X * x) (point.Y * y)

let translate (v: Vector2D<'Unit, 'Coordinates>) (p: Point2D<'Unit, 'Coordinates>) = p + v

let rotateAround
    (reference: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (point: Point2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =
    let c = Angle.cos angle
    let s = Angle.sin angle
    let deltaX = point.X - reference.X
    let deltaY = point.Y - reference.Y

    xy (reference.X + c * deltaX - s * deltaY) (reference.Y + s * deltaX + c * deltaY)

let placeIn (frame: Frame2D<'Unit, 'Coordinates>) (point: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    let i = frame.XDirection
    let j = frame.YDirection

    xy (frame.Origin.X + point.X * i.X + point.Y * j.X) (frame.Origin.Y + point.X * i.Y + point.Y * j.Y)

(* Queries *)

let distanceSquaredTo (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let dx = (p1.X - p2.X)
    let dy = (p1.Y - p2.Y)
    dx * dx + dy * dy

let distanceTo p1 p2 : Length<'Unit> = distanceSquaredTo p1 p2 |> Length.sqrt

let midpoint (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    xy ((p1.X + p2.X) / 2.) ((p1.Y + p2.Y) / 2.)

let round (p: Point2D<'Unit, 'Coordinates>) =
    xy (Length.round p.X) (Length.round p.Y)


/// Be careful with the vector arguments. This function is written with piping in mind. The first point is the
/// target location. The second point is the starting location
let vectorTo
    (target: Point2D<'Unit, 'Coordinates>)
    (from: Point2D<'Unit, 'Coordinates>)
    : Vector2D<'Unit, 'Coordinates> =
    target - from


(* Json *)

let fromList (list: float list) : Point2D<'Unit, 'Coordinates> option =
    match list with
    | [ x; y ] ->
        Some
        <| xy (Length<'Unit>.create x) (Length<'Unit>.create y)
    | _ -> None

let toList (point: Point2D<'Unit, 'Coordinates>) : float list = [ point.X.value (); point.Y.value () ]

(* Json transformations *)

type Transform() =
    interface ITypeTransform with
        member this.targetType() = (fun _ -> typeof<float32 list>) ()

        member this.toTargetType value =
            toList (value :?> Point2D<obj, obj>) :> obj

        member this.fromTargetType value =
            value :?> float list
            |> fromList
            |> Option.defaultValue (xy Length.zero Length.zero)
            :> obj

type ListTransform() =
    interface ITypeTransform with
        member this.targetType() = (fun _ -> typeof<float list list>) ()

        member this.toTargetType value =
            value :?> Point2D<obj, obj> list
            |> List.map toList
            :> obj

        member this.fromTargetType value =
            value :?> float list list
            |> List.map (
                fromList
                >> Option.defaultValue (xy Length.zero Length.zero)
            )
            :> obj
