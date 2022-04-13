[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Vector2D

open FSharp.Json

// ---- Builders ----

/// Construct a Vector2D object from the x and y lengths.
let xy (x: Length<'Unit>) (y: Length<'Unit>) : Vector2D<'Unit, 'Coordinates> = { X = x; Y = y }

/// Construct a vector given its local components within a particular frame
let xyIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (x: Length<'Unit>)
    (y: Length<'Unit>)
    : Vector2D<'Unit, 'Coordinates> =
    let i = frame.XDirection
    let j = frame.YDirection
    xy (x * i.X + y * j.X) (x * i.Y + y * j.Y)

let from (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = p2 - p1

/// Construct a vector with the given length in the given direction.
let withLength (a: Length<'Unit>) (d: Direction2D<'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    Internal.Vector2D.withLength a d

/// Construct a vector using polar coordinates coordinates given a length and angle
let rTheta (r: Length<'Unit>) (theta: Angle) : Vector2D<'Unit, 'Coordinates> =
    xy (r * (Angle.cos theta)) (r * (Angle.sin theta))

/// Construct a vector given its local polar components within a particular
let rThetaIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (r: Length<'Unit>)
    (theta: Angle)
    : Vector2D<'Unit, 'Coordinates> =
    let i = frame.XDirection
    let j = frame.YDirection
    let cosTheta = Angle.cos theta
    let sinTheta = Angle.sin theta
    xy (r * (cosTheta * i.X + sinTheta * j.X)) (r * (cosTheta * i.Y + sinTheta * j.Y))

/// Alias for `rTheta`
let polar (r: Length<'Unit>) (theta: Angle) : Vector2D<'Unit, 'Coordinates> = rTheta r theta

let zero<'Unit, 'Coordinates> : Vector2D<'Unit, 'Coordinates> = xy Length.zero Length.zero


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
    let largestComponent = max (Length.abs v.X) (Length.abs v.Y)

    if largestComponent = Length.zero then
        Length.zero

    else
        let scaledX = v.X / largestComponent
        let scaledY = v.Y / largestComponent

        let scaledLength =
            sqrt (scaledX * scaledX + scaledY * scaledY)
            
        scaledLength * largestComponent

/// Alias for `Vector2D.magnitude`
let length = magnitude

/// Get the X and Y components of a vector as a tuple.
let components (v: Vector2D<'Unit, 'Coordinates>) : Length<'Unit> * Length<'Unit> = (v.X, v.Y)

let x (v: Vector2D<'Unit, 'Coordinates>) : Length<'Unit> = v.X

let y (v: Vector2D<'Unit, 'Coordinates>) : Length<'Unit> = v.Y

// ---- Operators ----

/// This function is designed to be used in piping operators.
let plus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs + rhs

/// Find the sum of a list of vectors.
let sum (vectors: Vector2D<'Unit, 'Coordiante> list) : Vector2D<'Unit, 'Coordiantes> =
    let rec sumHelp sumX sumY vectors =
        match vectors with
        | v: Vector2D<'Unit, 'Coordinate> :: rest -> sumHelp (sumX + v.X) (sumY + v.Y) rest
        | [] -> xy sumX sumY

    sumHelp Length.zero Length.zero vectors



let minus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs - rhs

let times (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs * rhs

let cross (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Vector2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    (lhs.X * rhs.Y - lhs.Y * rhs.X)

/// Alias for `Vector2D.times`
let scaleBy = times

let dividedBy (rhs: float) (lhs: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = lhs / rhs

let neg (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = -v

/// Shorthand for `Vector2D.scaleBy 2`.
let twice (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = scaleBy 2. v

/// Shorthand for `Vector2D.scaleBy 0.5`.
let half (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = scaleBy 0.5 v


// ---- Modifiers ----

/// Scale a vector to a given length.
let scaleTo (scale: Length<'Unit>) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    let largestComponent = max (Length.abs v.X) (Length.abs v.Y)
    if largestComponent = Length.zero then
        zero

    else
        let scaledX = v.X / largestComponent
        let scaledY = v.Y / largestComponent

        let scaledLength =
            sqrt (scaledX * scaledX + scaledY * scaledY)

        xy (scale * scaledX / scaledLength) (scale * scaledY / scaledLength)


/// Rotate a vector counterclockwise by a given angle.
let rotateBy (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    xy (Angle.cos a * v.X - Angle.sin a * v.Y) (Angle.sin a * v.X + Angle.cos a * v.Y)

/// Rotate a vector counterClockwise by a given angle. Alias for `rotateBy`
let rotateByCounterClockwise = rotateBy

/// Rotate a vector clockwise by a given angle.
let rotateByClockwise (a: Angle) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = rotateBy -a v

/// Rotate the given vector 90 degrees counterclockwise;
///     Vector2D.rotateCounterclockwise vector
/// is equivalent to
///     Vector2D.rotateBy (Angle.degrees 90) vector
/// but is more efficient.
let rotateClockwise (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = xy -v.Y v.X

/// Rotate the given vector 90 degrees clockwise;
///     Vector2D.rotateClockwise vector
/// is equivalent to
///     Vector2D.rotateBy (Angle.degrees -90) vector
/// but is more efficient.
let rotateCounterclockwise (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = xy v.Y -v.X

/// Construct a vector perpendicular to the given vector, by rotating the given
/// vector 90 degrees counterclockwise. The constructed vector will have the same
/// length as the given vector. Alias for `Vector2D.rotateCounterclockwise`.
let perpendicularTo (givenVector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    rotateCounterclockwise givenVector

let normalize (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = scaleTo (Length.create<'Unit> 1.) v

/// Round the vector to the internal precision.
/// (Default is 8 digits past the decimal point)
let round (v: Vector2D<'Unit, 'Coordinates>) =
    xy (Length.round v.X) (Length.round v.Y)

/// Round the vector to a specified number of digits
let roundTo (digits: int) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    xy (Length.roundTo digits v.X) (Length.roundTo digits v.Y)

/// Find the component of a vector in an arbitrary direction, for example
let componentIn (d: Direction2D<'Coordinates>) (v: Vector2D<'Unit, 'Coordiantes>) : Length<'Unit> =
    Internal.Vector2D.componentIn d v

/// Reverse the direction of a vector, negating its components.
let reverse (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = xy -v.X -v.Y

/// Alias for `Vector2D.reverse`
let negate = reverse

/// Mirror a vector across a given axis.
/// The position of the axis doesn't matter, only its orientation:
let mirrorAcross
    (axis: Axis2D<'AxisUnit, 'Coordinates>)
    (v: Vector2D<'Unit, 'Coordinates>)
    : Vector2D<'Unit, 'Coordinates> =
    let d = axis.Direction
    let a = 1. - 2. * d.Y * d.Y
    let b = 2. * d.X * d.Y
    let c = 1. - 2. * d.X * d.X
    xy (a * v.X + b * v.Y) (b * v.X + c * v.Y)

/// Find the projection of a vector in a particular direction. Conceptually,
/// this means splitting the original vector into a portion parallel to the given
/// direction and a portion perpendicular to it, then returning the parallel
/// portion.
let projectionIn (d: Direction2D<'Coordinates>) (v: Vector2D<'Unit, 'Coordiantes>) : Vector2D<'Unit, 'Coordinates> =
    let projectedLength = v.X * d.X + v.Y * d.Y
    xy (projectedLength * d.X) (projectedLength * d.Y)

/// Project a vector onto an axis.
let projectOnto (axis: Axis2D<'Unit, 'Coordinates>) (v: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    let d = axis.Direction
    let projectedLength = v.X * d.X + v.Y * d.Y
    xy (projectedLength * d.X) (projectedLength * d.Y)

/// Take a vector defined in global coordinates, and return it expressed in
/// local coordinates relative to a given reference frame.
let relativeTo (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (v: Vector2D<'Unit, 'Coordinates>) =
    let dx = frame.XDirection
    let dy = frame.YDirection
    xy (v.X * dx.X + v.Y * dx.Y) (v.X * dy.X + v.Y * dy.Y)

/// Take a vector defined in local coordinates relative to a given reference
/// frame, and return that vector expressed in global coordinates.
let placeIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (v: Vector2D<'Unit, 'Coordinates>)
    : Vector2D<'Unit, 'Coordinates> =
    let dx = frame.XDirection
    let dy = frame.YDirection
    xy (v.X * dx.X + v.Y * dy.X) (v.X * dx.Y + v.Y * dy.Y)


// ---- Queries ----

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
let direction (v: Vector2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> option = Direction2D.xyLength v.X v.Y

/// Compare two vectors within a tolerance. Returns true if the difference
/// between the two given vectors has magnitude less than the given tolerance.
let equalWithin
    (tolerance: Length<'Unit>)
    (lhs: Vector2D<'Unit, 'Coordinates>)
    (rhs: Vector2D<'Unit, 'Coordinates>)
    : bool =
    if tolerance > Length.zero then
        let nx = (rhs.X - lhs.X) / tolerance
        let ny = (rhs.Y - lhs.Y) / tolerance
        nx * nx + ny * ny <= 1.

    else if tolerance = Length.zero then
        lhs.X = rhs.X && lhs.Y = rhs.Y

    else
        false

// ---- Json ----

let fromList (list: float list) : Vector2D<'Unit, 'Coordinates> option =
    match list with
    | [ x; y ] ->
        xy (Length<'Unit>.create x) (Length<'Unit>.create y)
        |> Some

    | _ -> None

let toList (vector: Vector2D<'Unit, 'Coordinates>) : float list =
    [ Length.unpack vector.X
      Length.unpack vector.Y ]


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
