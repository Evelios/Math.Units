module Geometry.Ellipse2D

open Units

// ---- Builders ----

let from
    (centerPoint: Point2D<'Units, 'Coordinates>)
    (xDirection: Direction2D<'Coordinates>)
    (xRadius: Quantity<'Units>)
    (yRadius: Quantity<'Units>)
    : Ellipse2D<'Units, 'Coordinates> =

    { Axes = Frame2D.withXDirection xDirection centerPoint
      XRadius = Length.abs xRadius
      YRadius = Length.abs yRadius }

// ---- Accessors ----

/// Get the X and Y axes of an ellipse as a `Frame2D`.
let axes (ellipse: Ellipse2D<'Units, 'Coordinates>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    Frame2D.copy ellipse.Axes

/// Get the center point of an ellipse.
let centerPoint (ellipse: Ellipse2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
    Frame2D.originPoint (axes ellipse)


/// Get the X axis of an ellipse.
let xAxis (ellipse: Ellipse2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> = Frame2D.xAxis (axes ellipse)


/// Get the Y axis of an ellipse.
let yAxis (ellipse: Ellipse2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> = Frame2D.yAxis (axes ellipse)


/// Get the radius of an ellipse along its X axis. This may be either the
/// minimum or maximum radius.
let xRadius (ellipse: Ellipse2D<'Units, 'Coordinates>) : Quantity<'Units> = ellipse.XRadius


/// Get the radius of an ellipse along its Y axis. This may be either the
/// minimum or maximum radius.
let yRadius (ellipse: Ellipse2D<'Units, 'Coordiantes>) : Quantity<'Units> = ellipse.YRadius


/// Get the direction of the ellipse's X axis.
let xDirection (ellipse: Ellipse2D<'Units, 'Coordinates>) : Direction2D<'Coordinates> =
    Frame2D.xDirection (axes ellipse)


/// Get the direction of an ellipse's Y axis.
let yDirection (ellipse: Ellipse2D<'Units, 'Coordinates>) : Direction2D<'Coordinates> =
    Frame2D.yDirection (axes ellipse)


/// Get the area of an ellipse.
let area (ellipse: Ellipse2D<'Units, 'Coordinates>) : Quantity<Squared<'Units>> =
    let x: Quantity<'Units> = xRadius ellipse
    let y: Quantity<'Units> = yRadius ellipse

    Angle.pi.Value * (x * y)

// ---- Modifiers ----

/// Scale an ellipse about a given point by a given scale.
let scaleAbout
    (point: Point2D<'Units, 'Coordinates>)
    (scale: float)
    (ellipse: Ellipse2D<'Units, 'Coordinates>)
    : Ellipse2D<'Units, 'Coordinates> =
    let newCenterPoint =
        Point2D.scaleAbout point scale (centerPoint ellipse)

    let newAxes =
        if scale >= 0. then
            { Origin = newCenterPoint
              XDirection = xDirection ellipse
              YDirection = yDirection ellipse }

        else
            { Origin = newCenterPoint
              XDirection = Direction2D.reverse (xDirection ellipse)
              YDirection = Direction2D.reverse (yDirection ellipse) }

    { Axes = newAxes
      XRadius = Length.abs (scale * (xRadius ellipse))
      YRadius = Length.abs (scale * (yRadius ellipse)) }

///
let transformBy
    (axesTransformation: Frame2D<'Units, 'CoordinatesA, unit> -> Frame2D<'Units, 'CoordinatesB, unit>)
    (ellipse: Ellipse2D<'Units, 'CoordinatesA>)
    : Ellipse2D<'Units, 'CoordinatesB> =
    { Axes = axesTransformation ellipse.Axes
      XRadius = ellipse.XRadius
      YRadius = ellipse.YRadius }


/// Rotate an ellipse around a given point by a given angle.
let rotateAround
    (point: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (ellipse: Ellipse2D<'Units, 'Coordinates>)
    : Ellipse2D<'Units, 'Coordinates> =
    transformBy (Frame2D.rotateAround point angle) ellipse


/// Translate an ellipse by a given displacement.
let translateBy
    (displacement: Vector2D<'Units, 'Coordinates>)
    (ellipse: Ellipse2D<'Units, 'Coordinates>)
    : Ellipse2D<'Units, 'Coordinates> =
    transformBy (Frame2D.translateBy displacement) ellipse


/// Translate an ellipse in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Quantity<'Units>)
    (ellipse: Ellipse2D<'Units, 'Coordinates>)
    =
    translateBy (Vector2D.withQuantity distance direction) ellipse


///  Mirror an ellipse across a given axis. Note that if the axes of the original
/// ellipse form a [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
/// frame, then the axes of the mirrored ellipse will form a left-handed frame (and
/// vice versa).
let mirrorAcross
    (axis: Axis2D<'Units, 'Coordinates>)
    (ellipse: Ellipse2D<'Units, 'Coordinates>)
    : Ellipse2D<'Units, 'Coordinates> =
    transformBy (Frame2D.mirrorAcross axis) ellipse


/// Take an ellipse defined in global coordinates, and return it expressed in
/// local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (ellipse: Ellipse2D<'Units, 'GlobalCoordinates>)
    : Ellipse2D<'Units, 'LocalCoordinates> =

    transformBy (Frame2D.relativeTo frame) ellipse

/// Take an ellipse considered to be defined in local coordinates relative to a
/// given reference frame, and return that circle expressed in global coordinates.
let placeIn
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (ellipse: Ellipse2D<'Units, 'GlobalCoordinates>)
    : Ellipse2D<'Units, 'LocalCoordinates> =

    transformBy (Frame2D.placeIn frame) ellipse
