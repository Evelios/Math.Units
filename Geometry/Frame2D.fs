[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Frame2D

open Units


// ---- Builders ----

let atOrigin<'Units, 'Coordinates, 'Defines> : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = { X = Quantity.zero; Y = Quantity.zero }
      XDirection = Direction2D.x
      YDirection = Direction2D.y }

let atPoint (point: Point2D<'Units, 'Coordinates>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = point
      XDirection = Direction2D.x
      YDirection = Direction2D.y }

/// Construct a frame with the given Y axis direction, having the given origin
/// point. The X axis direction will be constructed by rotating the given
/// direction 90 degrees clockwise.
let withXDirection xDirection origin =
    { Origin = origin
      XDirection = xDirection
      YDirection = Direction2D.rotateCounterclockwise xDirection }

/// Construct a frame with the given Y axis direction, having the given origin
/// point. The X axis direction will be constructed by rotating the given Y
/// direction 90 degrees clockwise.
let withYDirection
    (givenDirection: Direction2D<'Coordinates>)
    (givenOrigin: Point2D<'Units, 'Coordinates>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = givenOrigin
      XDirection = givenDirection |> Direction2D.rotateClockwise
      YDirection = givenDirection }

let withAngle (angle: Angle) (origin: Point2D<'Units, 'Coordinates>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    withXDirection (Direction2D.fromAngle angle) origin

/// Construct a `Frame2d` given its X axis
/// `Frame2d.fromXAxis axis`
/// is equivalent to
/// `Frame2d.withXDirection (Axis2d.direction axis) (Axis2d.originPoint axis)`
let fromXAxis (givenAxis: Axis2D<'Units, 'Coordinates>) =
    withXDirection givenAxis.Direction givenAxis.Origin

/// Construct a `Frame2d` given its Y axis;
/// `Frame2d.fromYAxis axis` is equivalent to
/// `Frame2d.withYDirection (Axis2d.direction axis) (Axis2d.originPoint axis)`
let fromYAxis (givenAxis: Axis2D<'Units, 'Coordinates>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    withYDirection givenAxis.Direction givenAxis.Origin


// ---- Accessors ----

let originPoint (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Point2D<'Units, 'Coordinates> = frame.Origin

let xDirection (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Direction2D<'Coordinates> = frame.XDirection

let yDirection (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Direction2D<'Coordinates> = frame.YDirection

/// Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).
/// All predefined frames are right-handed, and most operations on frames preserve
/// handedness, so about the only ways to end up with a left-handed frame are by
/// constructing one explicitly with `unsafe` or by mirroring a right-handed frame.
let isRightHanded (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : bool =
    let x1 = Direction2D.xComponent frame.XDirection
    let y1 = Direction2D.yComponent frame.XDirection
    let x2 = Direction2D.xComponent frame.YDirection
    let y2 = Direction2D.yComponent frame.YDirection
    x1 * y2 - y1 * x2 > 0.

/// Get the X axis of a given frame (the axis formed from the frame's origin
/// point and X direction).
let xAxis (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Axis2D<'Units, 'Coordinates> =
    { Origin = frame.Origin
      Direction = frame.XDirection }

/// Get the Y axis of a given frame (the axis formed from the frame's origin
/// point and Y direction).
let yAxis (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Axis2D<'Units, 'Coordinates> =
    { Origin = frame.Origin
      Direction = frame.YDirection }


// ---- Modifiers ----

/// Reverse the X direction of a frame, leaving its Y direction and origin point
/// the same. Note that this will switch the
/// [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
/// of the frame.
let reverseX (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = originPoint frame
      XDirection = Direction2D.reverse (xDirection frame)
      YDirection = yDirection frame }

/// Reverse the Y direction of a frame, leaving its X direction and origin point
/// the same. Note that this will switch the
/// [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
/// of the frame.
let reverseY (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = originPoint frame
      XDirection = (xDirection frame)
      YDirection = Direction2D.reverse (yDirection frame) }

/// Move a frame so that it has the given origin point.
let moveTo
    (newOrigin: Point2D<'Units, 'Coordinates>)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = newOrigin
      XDirection = xDirection frame
      YDirection = yDirection frame }

/// Rotate a frame counterclockwise by a given angle around the frame's own
/// origin point. The resulting frame will have the same origin point, and its X and
/// Y directions will be rotated by the given angle.
let rotateBy (angle: Angle) (frame: Frame2D<'Units, 'Coordinates, 'Defines>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    let rotateDirection = Direction2D.rotateBy angle

    { Origin = originPoint frame
      XDirection = rotateDirection (xDirection frame)
      YDirection = rotateDirection (yDirection frame) }


/// Rotate a frame counterclockwise around a given point by a given angle. The
/// frame's origin point will be rotated around the given point by the given angle,
/// and its X and Y basis directions will be rotated by the given angle.
let rotateAround
    (centerPoint: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    let rotatePoint =
        Internal.Point2D.rotateAround centerPoint angle

    let rotateDirection = Direction2D.rotateBy angle

    { Origin = rotatePoint (originPoint frame)
      XDirection = rotateDirection (xDirection frame)
      YDirection = rotateDirection (yDirection frame) }


/// Translate a frame by a given displacement.
let translateBy
    (vector: Vector2D<'Units, 'Coordinates>)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin = Internal.Point2D.translateBy vector (originPoint frame)
      XDirection = xDirection frame
      YDirection = yDirection frame }

/// Translate a frame in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Quantity<'Units>)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    translateBy (Internal.Vector2D.withQuantity distance direction) frame

/// Translate a frame along one of its own axes by a given distance.
/// The first argument is a function that returns the axis to translate along, given
/// the current frame. The majority of the time this argument will be either
/// `Frame2d.xAxis` or `Frame2d.yAxis`. The second argument is the distance to
/// translate along the given axis.
let translateAlongOwn
    (axis: Frame2D<'Units, 'Coordinates, 'Defines1> -> Axis2D<'Units, 'Coordinates>)
    (distance: Quantity<'Units>)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines1>)
    : Frame2D<'Units, 'Coordinates, 'Defines2> =
    let frame =
        frame
        |> translateIn (axis frame).Direction distance

    { Origin = frame.Origin
      XDirection = frame.XDirection
      YDirection = frame.YDirection }

/// Mirror a frame across an axis.
/// Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
/// of the frame.
let mirrorAcross
    (axis: Axis2D<'Units, 'Coordinates>)
    (frame: Frame2D<'Units, 'Coordinates, 'Defines>)
    : Frame2D<'Units, 'Coordinates, 'Defines> =
    let mirrorPoint = Internal.Point2D.mirrorAcross axis
    let mirrorDirection = Direction2D.mirrorAcross axis

    { Origin = mirrorPoint (originPoint frame)
      XDirection = mirrorDirection (xDirection frame)
      YDirection = mirrorDirection (yDirection frame) }

/// Create a 'fresh copy' of a frame: one with the same origin point and X/Y
/// directions, but that can be used to define a different local coordinate system.
/// Sometimes useful in generic/library code. Despite the name, this is efficient:
/// it really just returns the value you passed in, but with a different type.
let copy (properties: Frame2D<'Units, 'Coordinates, 'Defines1>) : Frame2D<'Units, 'Coordinates, 'Defines2> =
    { Origin = properties.Origin
      XDirection = properties.XDirection
      YDirection = properties.YDirection }

/// Take two frames defined in global coordinates, and return the second one
/// expressed in local coordinates relative to the first.
let relativeTo<'Units, 'GlobalCoordinates, 'Defines, 'LocalCoordinates>
    (otherFrame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    : Frame2D<'Units, 'LocalCoordinates, 'Defines> =
    { Origin = Internal.Point2D.relativeTo otherFrame (originPoint frame)
      XDirection = Direction2D.relativeTo otherFrame (xDirection frame)
      YDirection = Direction2D.relativeTo otherFrame (yDirection frame) }

/// Take one frame defined in global coordinates and a second frame defined
/// in local coordinates relative to the first frame, and return the second frame
/// expressed in global coordinates.
let placeIn<'Units, 'GlobalCoordinates, 'Defines, 'LocalCoordinates>
    (reference: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    : Frame2D<'Units, 'LocalCoordinates, 'Defines> =
    { Origin = Internal.Point2D.placeIn reference frame.Origin
      XDirection = Direction2D.placeIn reference frame.XDirection
      YDirection = Direction2D.placeIn reference frame.YDirection }
