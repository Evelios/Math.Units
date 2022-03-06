[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Axis2D


// ---- Builders ----

let through
    (origin: Point2D<'Unit, 'Coordinates>)
    (direction: Direction2D<'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =
    { Origin = origin
      Direction = direction }

let x () : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.origin ()) (Direction2D.x ())

let y () : Axis2D<'Unit, 'Coordinates> =
    through (Point2D.origin ()) (Direction2D.y ())

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

    Direction2D<'Coordinates>.xyLength vector.X vector.Y
    |> Option.map (through first)


// ---- Accessors ----

let originPoint (axis: Axis2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = axis.Origin

let direction (axis: Axis2D<'Unit, 'Coordinates>) : Direction2D<'Coordinates> = axis.Direction


// ---- Modifiers ----

let reverse (axis: Axis2D<'Unit, 'Coordaintes>) : Axis2D<'Unit, 'Coordaintes> =
    through axis.Origin (Direction2D.reverse axis.Direction)

let moveTo (point: Point2D<'Unit, 'Coordinates>) (axis: Axis2D<'Unit, 'Coordinates>) : Axis2D<'Unit, 'Coordinates> =
    through point axis.Direction

let rotateAround
    (center: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (axis: Axis2D<'Unit, 'Coordinates>)
    : Axis2D<'Unit, 'Coordinates> =

    let rotatePoint = Point2D.rotateAround center angle
    let rotateDirection = Direction2D.rotateBy angle
    through (rotatePoint axis.Origin) (rotateDirection axis.Direction)
