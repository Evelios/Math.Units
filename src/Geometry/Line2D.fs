[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Line2D

(* Builders *)

let through (start: Point2D<'Unit, 'Coordinates>) (finish: Point2D<'Unit, 'Coordinates>) : Line2D<'Unit, 'Coordinates> =
    { Start = start; Finish = finish }

/// Create a line Starting at point in a particular direction and length
let fromPointAndVector (start: Point2D<'Unit, 'Coordinates>) (direction: Vector2D<'Unit, 'Coordinates>) =
    { Start = start
      Finish = start + direction }

let private toLineSegment (line: Line2D<'Unit, 'Coordinates>) : LineSegment2D<'Unit, 'Coordinates> =
    LineSegment2D.from line.Start line.Finish

(* Attributes *)

let direction (line: Line2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    Vector2D.normalize (line.Start - line.Finish)

let length (line: Line2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Point2D.distanceTo line.Start line.Finish


(* Modifiers *)

let round (l: Line2D<'Unit, 'Coordinates>) =
    through (Point2D.round l.Start) (Point2D.round l.Finish)


(* Queries *)

let pointClosestTo
    (point: Point2D<'Unit, 'Coordinates>)
    (line: Line2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =
    let v : Vector2D<'Unit, 'Coordinates> = line.Start |> Point2D.vectorTo point
    let lineDirection = direction line

    let alongVector =
        (Vector2D.dotProduct v (direction line)).value ()
        * lineDirection

    line.Start + alongVector


let distanceToPoint (point: Point2D<'Unit, 'Coordinates>) (line: Line2D<'Unit, 'Coordinates>) : Length<'Unit> =
    if line.Start = point || line.Finish = point then
        Length.zero
    else
        Point2D.distanceTo point (pointClosestTo point line)

let atPointInDirection
    (point: Point2D<'Unit, 'Coordinates>)
    (direction: Vector2D<'Unit, 'Coordinates>)
    : Line2D<'Unit, 'Coordinates> =
    through point (point + direction)

let perpThroughPoint
    (point: Point2D<'Unit, 'Coordinates>)
    (line: Line2D<'Unit, 'Coordinates>)
    : Line2D<'Unit, 'Coordinates> =
    atPointInDirection point (Vector2D.rotateBy (Angle.inDegrees 90.) (direction line))

let isPointOnLine (point: Point2D<'Unit, 'Coordinates>) (line: Line2D<'Unit, 'Coordinates>) =
    point = line.Start
    || point = line.Finish
    || point = pointClosestTo point line

let areParallel (first: Line2D<'Unit, 'Coordinates>) (second: Line2D<'Unit, 'Coordinates>) : bool =
    let d1 = direction first
    let d2 = direction second

    d1 = d2 || Vector2D.neg d1 = d2

let arePerpendicular (first: Line2D<'Unit, 'Coordinates>) (second: Line2D<'Unit, 'Coordinates>) =
    let d1 = (direction first)

    let d2 =
        Vector2D.rotateBy (Angle.pi / 2.) (direction second)

    d1 = d2 || Vector2D.neg d1 = d2

let intersect
    (first: Line2D<'Unit, 'Coordinates>)
    (second: Line2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> option =
    if areParallel first second then
        None
    else
        // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
        let p = first.Start
        let q = second.Start

        let r =
            first.Start |> Point2D.vectorTo first.Finish

        let s =
            second.Start |> Point2D.vectorTo second.Finish

        let t =
            Vector2D.crossProduct (q - p) s
            / Vector2D.crossProduct r s

        p + (t * r) |> Some
