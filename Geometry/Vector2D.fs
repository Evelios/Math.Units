[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Vector2D

open FSharp.Json

// ---- Builders ----

/// Construct a Vector2D object from the x and y lengths.
let xy (x: Length<'Unit>) (y: Length<'Unit>) : Vector2D<'Unit, 'Coordinates> = { X = x; Y = y }

/// Construct a vector using polar coordinates coordinates given a length and angle
let rTheta (r: Length<'Unit>) (theta: Angle) : Vector2D<'Unit, 'Coordinates> =
    xy (r * (Angle.cos theta)) (r * (Angle.sin theta))

/// Alias for `rTheta`
let polar r theta = rTheta r theta

let zero () : Vector2D<'Unit, 'Coordinates> = xy Length.zero Length.zero

// ---- Helper Builders ----

let private fromUnit conversion (x: float) (y: float) : Vector2D<Meters, 'Coordinates> =
    xy (conversion x) (conversion y)

let meters (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.meters x y
let pixels (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.cssPixels x y
let millimeters (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.millimeters x y
let centimeters (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.centimeters x y
let inches (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.inches x y
let feet (x: float) (y: float) : Vector2D<Meters, 'Coordinates> = fromUnit Length.feet x y

// ---- Accessors ----

let magnitude (v: Vector2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Length.sqrt ((Length.square v.X) + (Length.square v.Y))

// ---- Operators ----

/// This function is designed to be used in piping operators.
let plus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs + rhs

let minus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs - rhs

let times (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs * rhs

/// Alias for `times`
let scaleBy = times

let dividedBy (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs / rhs

let neg (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = -v

(* Modifiers *)

/// Scale a vector to a given length.
let scaleTo (length: Length<'Unit>) (vector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    scaleBy (length / magnitude vector) vector

/// Rotate a vector counterclockwise by a given angle.
let rotateBy (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    xy (Angle.cos a * v.X - Angle.sin a * v.Y) (Angle.sin a * v.X + Angle.cos a * v.Y)

/// Rotate a vector clockwise by a given angle.
let rotateByClockwise (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = rotateBy -a v

let normalize (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = v / (magnitude v).value ()

/// Round the vector to the internal precision.
/// (Default is 8 digits past the decimal point)
let round (v: Vector2D<'Unit, 'Coordinates>) =
    xy (Length.round v.X) (Length.round v.Y)

/// Round the vector to a specified number of digits
let roundTo (digits: int) (v: Vector2D<'Unit, 'Coordinates>) =
    xy (Length.roundTo digits v.X) (Length.roundTo digits v.Y)

(* Queries *)

/// Get the distance between two vectors squared. This function can be used to
/// optimize some algorithms because you remove a square root call from the
/// calculation which can be an expensive operation.
let distanceSquaredTo (p1: Vector2D<'Unit, 'Coordinates>) (p2: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let dx = (p1.X - p2.X)
    let dy = (p1.Y - p2.Y)
    dx * dx + dy * dy

let distanceTo p1 p2 : Length<'Unit> = distanceSquaredTo p1 p2 |> Length.sqrt

/// Get the vector that is the average of two vectors.
let midVector (p1: Vector2D<'Unit, 'Coordinates>) (p2: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    xy ((p1.X + p2.X) / 2.) ((p1.Y + p2.Y) / 2.)

let dotProduct (lhs: Vector2D<'Unit, 'Coordinates>) (rhs: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    ((lhs.X * rhs.X) + (lhs.Y * rhs.Y))

let crossProduct (lhs: Vector2D<'Unit, 'Coordinates>) (rhs: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    (lhs.X * rhs.Y) - (lhs.Y * rhs.X)

/// Get the direction the a vector is facing.
let direction (vector: Vector2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> option =
    Direction2D.xyLength vector.X vector.Y


// ---- Json ----

let fromList (list: float list) : Vector2D<'Unit, 'Coordinates> option =
    match list with
    | [ x; y ] ->
        xy (Length<'Unit>.create x) (Length<'Unit>.create y)
        |> Some

    | _ -> None

let toList (vector: Vector2D<'Unit, 'Coordinates>) : float list =
    [ vector.X.value (); vector.Y.value () ]


// ---- Json transformations ----

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
