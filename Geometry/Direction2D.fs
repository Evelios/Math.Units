[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Direction2D


// ---- Builders ----

/// Get a direction vector from the x and y components. This function takes
/// care of normalizing the x and y components into the unit direction vector.
/// This function also checks for the edge case where the x and y components
/// are both zero. In that case, the function returns `None`.
let xy (x: float) (y: float) : Direction2D<'Coordinates> option =
    if x = 0. && y = 0. then
        None
    else
        let magnitude = sqrt ((x * x) + (y * y))

        Some
            { Direction2D.X = x / magnitude
              Direction2D.Y = y / magnitude }

let fromComponents (x: float, y: float) : Direction2D<'Coordinates> option =
    if x = 0. && y = 0. then
        None
    else
        let magnitude = sqrt ((x * x) + (y * y))

        Some
            { Direction2D.X = x / magnitude
              Direction2D.Y = y / magnitude }


/// Get a direction vector from the x and y length components. This function takes
/// care of normalizing the x and y components into the unit direction vector.
/// This function also checks for the edge case where the x and y components
/// are both zero. In that case, the function returns `None`.
let xyLength (Length.Length x: Length<'Unit>) (Length.Length y: Length<'Unit>) : Direction2D<'Coordinates> option =
    xy x y

/// Create a direction vector from the x and y components. This function
/// doesn't perform either zero magnitude checks nor does it normalize the
/// input vectors. This function should only be used with input constants or
/// when you are sure that you aren't going to create a direction with an
/// invalid state.
let xyUnsafe (x: float) (y: float) : Direction2D<'Coordinates> =
    { Direction2D.X = x; Direction2D.Y = y }

// Create an angle counterclockwise from the positive X direction.
let fromAngle (angle: Angle) : Direction2D<'Coordinates> =
    xyUnsafe (Angle.cos angle) (Angle.sin angle)

let degrees (d: float) : Direction2D<'Coordinates> = Angle.degrees d |> fromAngle

let radians (r: float) : Direction2D<'Coordinates> = Angle.radians r |> fromAngle

let from
    (first: Point2D<'Unit, 'Coordinates>)
    (second: Point2D<'Unit, 'Coordinates>)
    : Direction2D<'Coordinates> option =
    let v = second - first
    xyLength v.X v.Y

// ---- Constants ----

let positiveX<'Coordinates> : Direction2D<'Coordinates> = xyUnsafe 1.0 0.0
let positiveY<'Coordinates> : Direction2D<'Coordinates> = xyUnsafe 0. 1.
let negativeX<'Coordinates> : Direction2D<'Coordinates> = xyUnsafe -1. 0.
let negativeY<'Coordinates> : Direction2D<'Coordinates> = xyUnsafe 0. -1.
let x<'Coordinates> : Direction2D<'Coordinates> = positiveX
let y<'Coordinates> : Direction2D<'Coordinates> = positiveY

// ---- Accessors ----

/// Convert a direction to a polar angle (the counterclockwise angle from the
/// positive X direction). The result will be in the range -180 to 180 degrees.
let toAngle (d: Direction2D<'Coordinates>) : Angle = Angle.radians (atan2 d.Y d.X)

/// {-| Get the X and Y components of a direction as a tuple (X, Y).
let toTuple (d: Direction2D<'Coordinates>) : float * float = (d.X, d.Y)

/// Get the X component of the direction.
let xComponent (d: Direction2D<'Coordinates>) : float = d.X

/// Get the X and Y component of the direction in a tuple.
let components (d: Direction2D<'Coordinates>) : float * float = d.X, d.Y

/// Get the Y component of the direction.
let yComponent (d: Direction2D<'Coordinates>) : float = d.Y

/// Convert a direction to a unitless vector of length 1.
let toVector (d: Direction2D<'Coordiantes>) : Vector2D<Unitless, 'Coordinates> =
    { X = Length.unitless d.X
      Y = Length.unitless d.Y }


// ---- Modifiers ----

let reverse (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> = xyUnsafe -direction.X -direction.Y

// Rotate a direction by 90 degrees counterclockwise.
let rotateCounterclockwise (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    xyUnsafe -direction.Y direction.X


// Rotate a direction by 90 degrees clockwise.
let rotateClockwise (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    xyUnsafe direction.Y -direction.X

/// Construct a direction perpendicular to the given direction, by rotating the
/// given direction 90 degrees counterclockwise. This is the same
/// `Direction2D.rotateBy (Angle.degrees 90)` but is more efficient.
/// Alias for `rotateCounterclockwise`.
let perpendicularTo (d: Direction2D<'Coordaintes>) : Direction2D<'Coordaintes> = { X = -d.Y; Y = d.X }

/// Rotate a direction counterclockwise by a given angle.
let rotateBy (angle: Angle) (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    let c = Angle.cos angle
    let s = Angle.sin angle
    xyUnsafe (c * direction.X - s * direction.Y) (s * direction.X + c * direction.Y)

/// Mirror a direction across a particular axis. Note that only the direction of
/// the axis affects the result, since directions are position-independent.
let mirrorAcross (axis: Axis2D<'Unit, 'Corodiantes>) (d: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    let a = axis.Direction
    let yy = 1. - 2. * a.Y * a.Y
    let xy = 2. * a.X * a.Y
    let xx = 1. - 2. * a.X * a.X
    xyUnsafe (yy * d.X + xy * d.Y) (xy * d.X + xx * d.Y)


/// Attempt to form a pair of perpendicular directions from the two given
/// vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):
/// * The first returned direction will be equal to the direction of the first
///   given vector
/// * The second returned direction will be as close as possible to the second
///   given vector while being perpendicular to the first returned direction
let orthonormalize
    (xVector: Vector2D<'Unit, 'Coordinates>)
    (xyVector: Vector2D<'Unit, 'Coordinatres>)
    : (Direction2D<'Coordinates> * Direction2D<'Coordinates>) option =
    let xDirectionOption = xyLength xVector.X xVector.Y

    match xDirectionOption with
    | Some xDirection ->
        let yDirection = perpendicularTo xDirection

        match Internal.Vector2D.componentIn yDirection xyVector with
        | p when p > Length.zero -> Some(xDirection, yDirection)
        | p when p < Length.zero -> Some(xDirection, reverse yDirection)
        | _ -> None

    | None -> None



// --- Queries ----

/// Compare two directions within an angular tolerance. Returns true if the
/// absolute value of the angle between the two given directions is less than the
/// given tolerance.
let equalWithin (angle: Angle) (rhs: Direction2D<'Coordinates>) (lhs: Direction2D<'Coordinates>) : bool =
    let relativeX = lhs.X * rhs.X + lhs.Y * rhs.Y
    let relativeY = lhs.X * rhs.Y - lhs.Y * rhs.X

    abs (atan2 relativeY relativeX)
    <= Angle.inRadians angle

/// Find the component of one direction in another direction. This is equal to
/// the cosine of the angle between the directions, or equivalently the dot product
/// of the two directions converted to unit vectors.
/// This is more general and flexible than using `xComponent` or `yComponent`, both
/// of which can be expressed in terms of `componentIn`; for example,
///     `Direction2d.xComponent direction`
/// is equivalent to
///     `Direction2d.componentIn Direction2d.x direction`.
let componentIn (d2: Direction2D<'Coordinates>) (d1: Direction2D<'Coordinates>) : float = d1.X * d2.X + d1.Y * d2.Y

///  Find the counterclockwise angle from the first direction to the
/// second. The result will be in the range -180 to 180 degrees
let angleFrom (d1: Direction2D<'Coordinates>) (d2: Direction2D<'Coordinates>) : Angle =
    let relativeX = d1.X * d2.X + d1.Y * d2.Y
    let relativeY = d1.X * d2.Y - d1.Y * d2.X
    atan2 relativeY relativeX |> Angle.radians


// ---- Coordinate Conversion ----

/// Take a direction defined in global coordinates, and return it expressed in
/// local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'GlobalCoordaintes, 'Defines>)
    (d: Direction2D<'GlobalCoordinates>)
    : Direction2D<'LocalCoordaintes> =

    let dx = frame.XDirection
    let dy = frame.YDirection
    xyUnsafe (d.X * dx.X + d.Y * dx.Y) (d.X * dy.X + d.Y * dy.Y)

/// Take a direction defined in local coordinates relative to a given reference
/// frame, and return that direction expressed in global coordinates.
let placeIn
    (reference: Frame2D<'Unit, 'GlobalCoordinates, 'Defines>)
    (direction: Direction2D<'GlobalCoordinates>)
    : Direction2D<'LocalCoordinates> =

    let dx = reference.XDirection
    let dy = reference.YDirection

    xyUnsafe (direction.X * dx.X + direction.Y * dy.X) (direction.X * dx.Y + direction.Y * dy.Y)
