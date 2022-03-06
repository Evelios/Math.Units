[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Point2D

open FSharp.Json


// ---- Builders ----

let xy (x: Length<'Unit>) (y: Length<'Unit>) : Point2D<'Unit, 'Coordinates> = { X = x; Y = y }

let rTheta (r: Length<'Unit>) (theta: Angle) : Point2D<'Unit, 'Coordinates> =
    xy (r * Angle.cos theta) (r * Angle.sin theta)

let polar r theta = rTheta r theta

let origin () : Point2D<'Unit, 'Coordinates> = xy Length.zero Length.zero


// ---- Helper Builder Functions ----

let private fromUnit conversion (x: float) (y: float) : Point2D<Meters, 'Coordinates> = xy (conversion x) (conversion y)
let meters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.meters x y
let pixels (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.cssPixels x y
let millimeters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.millimeters x y
let centimeters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.centimeters x y
let inches (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.inches x y
let feet (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.feet x y


// ---- Operators ----

/// This function is designed to be used in piping operators.
let plus (rhs: Vector2D<'Unit, 'Coordinates>) (lhs: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    lhs + rhs

let minus (rhs: Point2D<'Unit, 'Coordinates>) (lhs: Point2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    lhs - rhs

/// Be careful with the vector arguments. This function is written with piping in mind. The first point is the
/// target location. The second point is the starting location.
/// This is also an alias for `minus` and is `target - from`
let vectorTo
    (target: Point2D<'Unit, 'Coordinates>)
    (from: Point2D<'Unit, 'Coordinates>)
    : Vector2D<'Unit, 'Coordinates> =
    target - from

let neg (v: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = -v

let times (rhs: float) (lhs: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = lhs * rhs

/// Alias for `times`
let scaleBy = times

let dividedBy (rhs: float) (lhs: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = lhs / rhs


// ---- Accessors ----

let magnitude (v: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Length.sqrt ((Length.square v.X) + (Length.square v.Y))


// ---- Conversions ----

let toVector (point: Point2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> = Vector2D.xy point.X point.Y


// ---- Modifiers ----

/// Scale a point to a given length.
let scaleTo (length: Length<'Unit>) (point: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    scaleBy (length / magnitude point) point

/// Rotate a point counterclockwise by a given angle.
let rotateBy (a: Angle) (p: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    xy (Angle.cos a * p.X - Angle.sin a * p.Y) (Angle.sin a * p.X + Angle.cos a * p.Y)

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

// ---- Queries ----

let distanceSquaredTo (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let dx = (p1.X - p2.X)
    let dy = (p1.Y - p2.Y)
    dx * dx + dy * dy

let distanceTo p1 p2 : Length<'Unit> = distanceSquaredTo p1 p2 |> Length.sqrt

let midpoint (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    xy ((p1.X + p2.X) / 2.) ((p1.Y + p2.Y) / 2.)

let lerp
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (percent: Percent)
    : Point2D<'Unit, 'Coordinates> =
    xy ((p1.X + p2.X) * (Percent.asRatio percent)) ((p1.Y + p2.Y) * (Percent.asRatio percent))

/// Get the direction the a point is facing.
let direction (point: Point2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> option =
    Direction2D.xyLength point.X point.Y

/// Round the point to the internal precision.
/// (Default is 8 digits past the decimal point)
let round (p: Point2D<'Unit, 'Coordinates>) =
    xy (Length.round p.X) (Length.round p.Y)

/// Round the point to a specified number of digits
let roundTo (digits: int) (p: Point2D<'Unit, 'Coordinates>) =
    xy (Length.roundTo digits p.X) (Length.roundTo digits p.Y)

let circumcenterHelp
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    (a: Length<'Unit>)
    (b: Length<'Unit>)
    (c: Length<'Unit>)
    =
    let bc = b * c

    if bc = Length<'Unit * 'Unit>.create 0. then
        None

    else
        let bx = p3.X - p2.X
        let by = p3.Y - p2.Y
        let cx = p1.X - p3.X
        let cy = p1.Y - p3.Y
        let sinA = (bx * cy - by * cx) / bc

        if sinA = 0. then
            None

        else
            let ax = p2.X - p1.X
            let ay = p2.Y - p1.Y
            let cosA = (bx * cx + by * cy) / bc
            let scale = cosA / (2. * sinA)

            xy (p1.X + 0.5 * ax + scale * ay) (p1.Y + 0.5 * ay - scale * ax)
            |> Some

let circumcenter
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    =
    let a = distanceTo p1 p2
    let b = distanceTo p2 p3
    let c = distanceTo p3 p1

    if a >= b then
        if a >= c then
            circumcenterHelp p1 p2 p3 a b c

        else
            circumcenterHelp p3 p1 p2 c a b

    else if b >= c then
        circumcenterHelp p2 p3 p1 b c a

    else
        circumcenterHelp p3 p1 p2 c a b

// ---- Json ----

let fromList (list: float list) : Point2D<'Unit, 'Coordinates> option =
    match list with
    | [ x; y ] ->
        Some
        <| xy (Length<'Unit>.create x) (Length<'Unit>.create y)
    | _ -> None

let toList (point: Point2D<'Unit, 'Coordinates>) : float list =
    [ Length.unpack point.X
      Length.unpack point.Y ]


// ---- Json transformations ----

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
