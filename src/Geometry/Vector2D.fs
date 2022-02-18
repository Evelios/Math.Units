[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Vector2D

open FSharp.Json

(* Builders *)

/// Construct a Vector2D object from the x and y lengths.
let xy (x: Length<'Unit>) (y: Length<'Unit>) : Vector2D<'Unit, 'Coordinates> = { X = x; Y = y }

/// Construct a vector from the x and y lengths in meters using float constants.
let meters (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = xy (Length.meters x) (Length.meters y)

/// Construct a vector using polar coordinates coordinates given a length and angle
let rTheta (r: Length<'Unit>) (theta: Angle) : Vector2D<'Unit, 'Coordinates> =
    { X = r * (Angle.cos theta)
      Y = r * (Angle.sin theta) }

/// Alias for `rTheta`
let polar r theta = rTheta r theta

(* Accessors *)

let magnitude (v: Vector2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Length.sqrt ((Length.square v.X) + (Length.square v.Y))

(* Operators *)

/// This function is designed to be used in piping operators.
let plus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs + rhs

let minus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs - rhs

let times (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs * rhs

let dividedBy (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs / rhs

let neg (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = -v

(* Modifiers *)

/// Alias for `times`
let scaleBy = times

/// Scale a vector to a given length.
let scaleTo (length: Length<'Unit>) (vector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    scaleBy (length / magnitude vector) vector

/// Rotate a vector counterclockwise by a given angle.
let rotateBy (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    { X = Angle.cos a * v.X - Angle.sin a * v.Y
      Y = Angle.sin a * v.X + Angle.cos a * v.Y }

/// Rotate a vector clockwise by a given angle.
let rotateByClockwise (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = rotateBy -a v

let normalize (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = v / (magnitude v).value ()

/// Round the vector to the nearest integer coordinates
let round (p: Vector2D<'Unit, 'Coordinates>) =
    xy (Length.round p.X) (Length.round p.Y)
    
(* Queries *)

let distanceSquaredTo (p1: Vector2D<'Unit, 'Coordinates>) (p2: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let dx = (p1.X - p2.X)
    let dy = (p1.Y - p2.Y)
    dx * dx + dy * dy

let distanceTo p1 p2 : Length<'Unit> = distanceSquaredTo p1 p2 |> Length.sqrt

let midVector (p1: Vector2D<'Unit, 'Coordinates>) (p2: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    xy ((p1.X + p2.X) / 2.) ((p1.Y + p2.Y) / 2.)

let dotProduct (lhs: Vector2D<'Unit, 'Coordinates>) (rhs: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    ((lhs.X * rhs.X) + (lhs.Y * rhs.Y))

let crossProduct (lhs: Vector2D<'Unit, 'Coordinates>) (rhs: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    (lhs.X * rhs.Y) - (lhs.Y * rhs.X)

/// Get the direction the a vector is facing.
let direction (vector: Vector2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> option =
    if magnitude vector = Length.zero then
        None
    else
        let normalized = normalize vector
        Some(Direction2D.xy (normalized.X.value ()) (normalized.Y.value ()))


(* Json *)

let fromList (list: float list) : Vector2D<'Unit, 'Coordinates> option =
    match list with
    | [ x; y ] ->
        xy (Length<'Unit>.create x) (Length<'Unit>.create y)
        |> Some

    | _ -> None

let toList (vector: Vector2D<'Unit, 'Coordinates>) : float list =
    [ vector.X.value (); vector.Y.value () ]


(* Json transformations *)

type Transform() =
    interface ITypeTransform with
        member this.targetType() = (fun _ -> typeof<float32 list>) ()

        member this.toTargetType value =
            toList (value :?> Vector2D<obj, obj>) :> obj

        member this.fromTargetType value =
            value :?> float list
            |> fromList
            |> Option.defaultValue (xy Length.zero Length.zero)
            :> obj

type ListTransform() =
    interface ITypeTransform with
        member this.targetType() = (fun _ -> typeof<float list list>) ()

        member this.toTargetType value =
            value :?> Vector2D<obj, obj> list
            |> List.map toList
            :> obj

        member this.fromTargetType value =
            value :?> float list list
            |> List.map (
                fromList
                >> Option.defaultValue (xy Length.zero Length.zero)
            )
            :> obj
