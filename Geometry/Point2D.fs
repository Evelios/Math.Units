[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Point2D

open FSharp.Json


// ---- Builders ----

let xy (x: Length<'Unit>) (y: Length<'Unit>) : Point2D<'Unit, 'Coordinates> = { X = x; Y = y }

let rTheta (r: Length<'Unit>) (theta: Angle) : Point2D<'Unit, 'Coordinates> =
    xy (r * Angle.cos theta) (r * Angle.sin theta)

let polar r theta = rTheta r theta

let origin<'Unit, 'Coordinates> : Point2D<'Unit, 'Coordinates> = xy Length.zero Length.zero

let unsafe<'Unit, 'Coordinates> (x: float) (y: float) : Point2D<'Unit, 'Coordinates> =
    { X = Length.create<'Unit> x
      Y = Length.create<'Unit> y }

// ---- Helper Builder Functions ----

let private fromUnit conversion (x: float) (y: float) : Point2D<'Unit, 'Coordinates> = xy (conversion x) (conversion y)
let meters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.meters x y
let pixels (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.cssPixels x y
let millimeters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.millimeters x y
let centimeters (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.centimeters x y
let inches (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.inches x y
let feet (x: float) (y: float) : Point2D<Meters, 'Coordinates> = fromUnit Length.feet x y
let unitless (x: float) (y: float) : Point2D<Unitless, 'Coordinates> = fromUnit Length.unitless x y


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

let x (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> = p.X

let y (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> = p.Y

let magnitude (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Length.sqrt ((Length.square p.X) + (Length.square p.Y))




/// Find the centroid (average) of one or more points, by passing the first
/// point and then all remaining points. This allows this function to return a
/// `Point2d` instead of a `Maybe Point2d`. You would generally use `centroid`
/// within a `case` expression.
/// Alternatively, you can use [`centroidN`](#centroidN) instead.
let centroid
    (p0: Point2D<'Unit, 'Coordinates>)
    (rest: Point2D<'Unit, 'Coordinates> list)
    : Point2D<'Unit, 'Coordinates> =

    let rec centroidHelp
        (x0: Length<'Unit>)
        (y0: Length<'Unit>)
        (count: int)
        (dx: Length<'Unit>)
        (dy: Length<'Unit>)
        (points: Point2D<'Unit, 'Coordinates> list)
        =

        match points with
        | p :: remaining -> centroidHelp x0 y0 (count + 1) (dx + (p.X - x0)) (dy + (p.Y - y0)) remaining

        | [] -> xy (x0 + dx / float count) (y0 + dy / float count)

    centroidHelp p0.X p0.Y 1 Length.zero Length.zero rest


/// Like `centroid`, but lets you work with any kind of data as long as a point
/// can be extracted/constructed from it. For example, to get the centroid of a
/// bunch of vertices.
let centroidOf
    (toPoint: 'a -> Point2D<'Unit, 'Coordinates>)
    (first: 'a)
    (rest: 'a list)
    : Point2D<'Unit, 'Coordinates> =

    let rec centroidOfHelp
        (x0: Length<'Unit>)
        (y0: Length<'Unit>)
        (count: int)
        (dx: Length<'Unit>)
        (dy: Length<'Unit>)
        (values: 'a list)
        : Point2D<'Unit, 'Coordiantes> =

        match values with
        | next :: remaining ->
            let p = toPoint next

            centroidOfHelp x0 y0 (count + 1) (dx + (p.X - x0)) (dy + (p.Y - y0)) remaining

        | [] -> xy (x0 + dx / float count) (y0 + dy / float count)

    let p0 = toPoint first
    centroidOfHelp p0.X p0.Y 1 Length.zero Length.zero rest

/// Find the centroid of three points
/// `Point2D.centroid3d p1 p2 p3` is equivalent to
/// `Point2d.centroid p1 [ p2, p3 ]`
/// but is more efficient.
let centroid3
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =

    xy (p1.X + (p2.X - p1.X) / 3. + (p3.X - p1.X) / 3.) (p1.Y + (p2.Y - p1.Y) / 3. + (p3.Y - p1.Y) / 3.)

/// Find the centroid of a list of _N_ points. If the list is empty, returns
/// `Nothing`. If you know you have at least one point, you can use
/// [`centroid`](#centroid) instead to avoid the `Option`.

let centroidN (points: Point2D<'Unit, 'Coordinates> list) : Point2D<'Unit, 'Coordinates> option =
    match points with
    | first :: rest -> Some(centroid first rest)
    | [] -> None


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
    Internal.Point2D.rotateAround reference angle point

let placeIn
    (frame: Frame2D<'Unit, 'GlobalCoordinates, 'Defines>)
    (point: Point2D<'Unit, 'GlobalCoordinates>)
    : Point2D<'Unit, 'LocalCoordinates> =
    Internal.Point2D.placeIn frame point

/// Translate a point in a given direction by a given distance.
let translateIn
    (d: Direction2D<'Coordinates>)
    (distance: Length<'Unit>)
    (p: Point2D<'Unit, 'Coordiantes>)
    : Point2D<'Unit, 'Coordiantes> =
    xy (p.X + distance * d.X) (p.Y + distance * d.Y)

let translateBy (v: Vector2D<'Unit, 'Coordiantes>) (p: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    Internal.Point2D.translateBy v p

/// Mirror a point across an axis. The result will be the same distance from the
/// axis but on the opposite side.
let mirrorAcross (axis: Axis2D<'Unit, 'Coordinates>) (p: Point2D<'Unit, 'Corodinates>) : Point2D<'Unit, 'Coordinates> =
    Internal.Point2D.mirrorAcross axis p

// ---- Queries ----

/// Compare two points within a tolerance. Returns true if the distance
/// between the two given points is less than the given tolerance.
let equalWithin (eps: Length<'Unit>) (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : bool =
    if eps > Length.zero then
        let nx = (p2.X - p1.X) / eps
        let ny = (p2.Y - p1.Y) / eps
        nx * nx + ny * ny <= 1.

    else if eps = Length.zero then
        p1.X = p2.X && p1.Y = p2.Y

    else
        false

/// Find the squared distance from the first point to the second.
let distanceSquaredTo (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let dx = (p1.X - p2.X)
    let dy = (p1.Y - p2.Y)
    dx * dx + dy * dy

/// Find the distance from the first point to the second.
let distanceTo (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    let deltaX = p2.X - p1.X
    let deltaY = p2.Y - p1.Y

    let largestComponent =
        max (Length.abs deltaX) (Length.abs deltaY)

    if largestComponent = Length.zero then
        Length.zero

    else
        let scaledX = deltaX / largestComponent
        let scaledY = deltaY / largestComponent

        let scaledLength =
            sqrt (scaledX * scaledX + scaledY * scaledY)

        scaledLength * largestComponent

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

let private circumcenterHelp
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    (_: Length<'Unit>)
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

/// Project a point perpendicularly onto an axis.
/// The axis does not have to pass through the origin:
let projectOnto (axis: Axis2D<'Unit, 'Coordinates>) (p: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    let p0 = axis.Origin
    let d = axis.Direction
    let distance = (p.X - p0.X) * d.X + (p.Y - p0.Y) * d.Y
    xy (p0.X + distance * d.X) (p0.Y + distance * d.Y)


// ---- Queries ----


/// Construct a point by interpolating from the first given point to the second,
/// based on a parameter that ranges from zero to one.
/// You can pass values less than zero or greater than one to extrapolate.
let interpolateFrom (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) (t: float) =
    if t <= 0.5 then
        xy (p1.X + t * (p2.X - p1.X)) (p1.Y + t * (p2.Y - p1.Y))
    else
        xy (p2.X + (1. - t) * (p1.X - p2.X)) (p2.Y + (1. - t) * (p1.Y - p2.Y))

/// Construct a point along an axis at a particular distance from the axis'
/// origin point.
/// Positive and negative distances will be interpreted relative to the direction of
/// the axis.
let along (axis: Axis2D<'Unit, 'Coordinates>) (distance: Length<'Unit>) : Point2D<'Unit, 'Coordinates> =
    let p0 = axis.Origin
    let d = axis.Direction
    xy (p0.X + distance * d.X) (p0.Y + distance * d.Y)

///  Determine how far along an axis a particular point lies. Conceptually, the
/// point is projected perpendicularly onto the axis, and then the distance of this
/// projected point from the axis' origin point is measured. The result will be
/// positive if the projected point is ahead the axis' origin point and negative if
/// it is behind, with 'ahead' and 'behind' defined by the direction of the axis.
let signedDistanceAlong (axis: Axis2D<'Unit, 'Coordinates>) (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    let p0 = axis.Origin
    let d = axis.Direction
    ((p.X - p0.X) * d.X + (p.Y - p0.Y) * d.Y)

/// Find the perpendicular distance of a point from an axis. The result
/// will be positive if the point is to the left of the axis and negative if it is
/// to the right, with the forwards direction defined by the direction of the axis.
let signedDistanceFrom (axis: Axis2D<'Unit, 'Coordinates>) (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    let p0 = axis.Origin
    let d = axis.Direction
    ((p.Y - p0.Y) * d.X - (p.X - p0.X) * d.Y)

/// Perform a uniform scaling about the given center point. The center point is
/// given first and the point to transform is given last. Points will contract or
/// expand about the center point by the given scale. Scaling by a factor of 1 is a
/// no-op, and scaling by a factor of 0 collapses all points to the center point.
/// Avoid scaling by a negative scaling factor - while this may sometimes do what
/// you want it is confusing and error prone. Try a combination of mirror and/or
/// rotation operations instead.
let scaleAbout
    (p0: Point2D<'Unit, 'Coordinates>)
    (k: float)
    (p: Point2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =
    xy (p0.X + k * (p.X - p0.X)) (p0.Y + k * (p.Y - p0.Y))


// ---- Coordinate Conversions ----

/// Construct a point given its local coordinates within a particular frame:
let xyIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (x: Length<'Unit>)
    (y: Length<'Unit>)
    : Point2D<'Unit, 'Coordinates> =
    let p0 = frame.Origin
    let i = frame.XDirection
    let j = frame.YDirection
    xy (p0.X + x * i.X + y * j.X) (p0.Y + x * i.Y + y * j.Y)

/// Construct a point given its local polar coordinates within a particular
/// frame.
let rThetaIn (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (r: Length<'Unit>) (theta: Angle) =
    let p0 = frame.Origin
    let i = frame.XDirection
    let j = frame.YDirection
    let x = r * Angle.cos theta
    let y = r * Angle.sin theta
    xy (p0.X + x * i.X + y * j.X) (p0.Y + x * i.Y + y * j.Y)

/// Find the X coordinate of a point relative to a given frame.
let xCoordinateIn (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    let p0 = frame.Origin
    let d = frame.XDirection
    ((p.X - p0.X) * d.X + (p.Y - p0.Y) * d.Y)

/// Find the Y coordinate of a point relative to a given frame.
let yCoordinateIn (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> =
    let p0 = frame.Origin
    let d = frame.YDirection
    ((p.X - p0.X) * d.X + (p.Y - p0.Y) * d.Y)

/// Get the X and Y coordinates of a point as a tuple.
/// Point2d.coordinates (Point2d.meters 2 3)
let coordinates (p: Point2D<'Unit, 'Coordinates>) : Length<'Unit> * Length<'Unit> = (p.X, p.Y)

/// Get the X and Y coordinates of a point relative to a given frame, as a
/// tuple; these are the coordinates the point would have as viewed by an observer
/// in that frame.
let coordinatesIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (p: Point2D<'Unit, 'Coordinates>)
    : Length<'Unit> * Length<'Unit> =
    let p0 = frame.Origin
    let dx = frame.XDirection
    let dy = frame.YDirection
    let deltaX = p.X - p0.X
    let deltaY = p.Y - p0.Y
    ((deltaX * dx.X + deltaY * dx.Y), (deltaX * dy.X + deltaY * dy.Y))

/// Take a point defined in global coordinates, and return it expressed in local
/// coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'GlobalCoordinates, 'Defines>)
    (p: Point2D<'Unit, 'GlobalCoordinates>)
    : Point2D<'Unit, 'LocalCoordinates> =
    Internal.Point2D.relativeTo frame p


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
