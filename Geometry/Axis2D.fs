[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Axis2D

open Units


// ---- Builders ----

let through
    (origin: Point2D<'Unit, 'Coordinates>)
    (direction: Direction2D<'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    { Origin = origin
      Direction = direction }

let x<'Unit, 'Coordinates> : Axis2D<'Unit, 'Coordinates> =
    through { X = Quantity.zero; Y = Quantity.zero } Direction2D.x

let y<'Unit, 'Coordinates> : Axis2D<'Unit, 'Coordinates> =
    through { X = Quantity.zero; Y = Quantity.zero } Direction2D.y

let withDirection
    (direction: Direction2D<'Coordinates>)
    (point: Point2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    { Origin = point
      Direction = direction }

let throughPoints
    (first: Point2D<'Unit, 'Coordinates>)
    (second: Point2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> option =

    let vector = (second - first)

    Direction2D.xyQuantity vector.X vector.Y
    |> Option.map (through first)


// ---- Accessors ----

/// Get the origin point of the axis.
let originPoint (axis: Axis2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = axis.Origin

/// Get the direction of the axis.
let direction (axis: Axis2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> = axis.Direction


// ---- Modifiers ----

/// Reverse the direction of the axis.
let reverse (axis: Axis2D<'Unit, 'Coordaintes>) : Axis2D<'Unit, 'Coordaintes> =
    through axis.Origin (Direction2D.reverse axis.Direction)

/// Move the origin to have a different origin point but the same direction.
let moveTo (point: Point2D<'Unit, 'Coordinates>) (axis: Axis2D<'Unit, 'Coordinates>) : Axis2D<'Unit, 'Coordinates> =
    through point axis.Direction

/// Rotate an axis around a given center point by a given angle. Rotates the
/// axis' origin point around the given point by the given angle and the axis'
/// direction by the given angle.
let rotateAround
    (center: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =

    let rotatePoint =
        Internal.Point2D.rotateAround center angle

    let rotateDirection = Direction2D.rotateBy angle
    through (rotatePoint axis.Origin) (rotateDirection axis.Direction)

/// Rotate an axis around its own origin point by the given angle.
let rotateBy (angle: Angle) (axis: Axis2D<'Unit, 'Coordinates>) : Axis2D<'Unit, 'Coordinates> =
    through (originPoint axis) (Direction2D.rotateBy angle (direction axis))


/// Translate an axis by a given displacement. Applies the given displacement to
/// the axis' origin point and leaves the direction unchanged.
let translateBy
    (vector: Vector2D<'Unit, 'Coordinates>)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.translateBy vector axis.Origin) axis.Direction


/// Translate an axis in a given direction by a given distance.
let translateIn
    (translationDirection: Direction2D<'Coordinates>)
    (distance: Quantity<'Unit>)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    translateBy (Vector2D.withLength distance translationDirection) axis


/// Mirror one axis across another. The axis to mirror across is given first and
/// the axis to mirror is given second.
///     Axis2D.mirrorAcross Axis2D.x exampleAxis
///     --> Axis2D.through (Point2D.meters 1. -3.)
///     -->     (Direction2D.degrees -30.)
let mirrorAcross
    (otherAxis: Axis2D<'Unit, 'Coordinates>)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.mirrorAcross otherAxis axis.Origin) (Direction2D.mirrorAcross otherAxis axis.Direction)

/// Take an axis defined in global coordinates, and return it expressed in local
/// coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.relativeTo frame axis.Origin) (Direction2D.relativeTo frame axis.Direction)


/// Take an axis defined in local coordinates relative to a given reference
/// frame, and return that axis expressed in global coordinates.
let placeIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.placeIn frame axis.Origin) (Direction2D.placeIn frame axis.Direction)
