[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Line2D

open Units

// ---- Builders ----

let through (start: Point2D<'Units, 'Coordinates>) (finish: Point2D<'Units, 'Coordinates>) : Line2D<'Units, 'Coordinates> =
    { Start = start; Finish = finish }

/// Create a line Starting at point in a particular direction and length
let fromPointAndVector (start: Point2D<'Units, 'Coordinates>) (direction: Vector2D<'Units, 'Coordinates>) =
    { Start = start
      Finish = start + direction }

let private toLineSegment (line: Line2D<'Units, 'Coordinates>) : LineSegment2D<'Units, 'Coordinates> =
    LineSegment2D.from line.Start line.Finish


// ---- Attributes ----

let direction (line: Line2D<'Units, 'Coordinates>) : Direction2D<'Coordinates> option =
    Vector2D.direction (Vector2D.from line.Start line.Finish)

let length (line: Line2D<'Units, 'Coordinates>) : Quantity<'Units> =
    Point2D.distanceTo line.Start line.Finish

let axis (line: Line2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> option =
    Axis2D.throughPoints line.Start line.Finish


// ---- Modifiers ----

let round (l: Line2D<'Units, 'Coordinates>) =
    through (Point2D.round l.Start) (Point2D.round l.Finish)


// ---- Queries ----

// Get the point closest to the line. This is the point projected onto that line.
let pointClosestTo
    (point: Point2D<'Units, 'Coordinates>)
    (line: Line2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> =
    match axis line with
    | Some lineAxis -> Point2D.projectOnto lineAxis point
    | None -> line.Start

let distanceToPoint (point: Point2D<'Units, 'Coordinates>) (line: Line2D<'Units, 'Coordinates>) : Quantity<'Units> =
    if line.Start = point || line.Finish = point then
        Quantity.zero
    else
        Point2D.distanceTo point (pointClosestTo point line)

let atPointInDirection
    (point: Point2D<'Units, 'Coordinates>)
    (direction: Vector2D<'Units, 'Coordinates>)
    : Line2D<'Units, 'Coordinates> =
    through point (point + direction)

let isPointOnLine (point: Point2D<'Units, 'Coordinates>) (line: Line2D<'Units, 'Coordinates>) =
    match axis line with
    | None -> point = line.Start
    | Some lineAxis -> point = Point2D.projectOnto lineAxis point

let areParallel (first: Line2D<'Units, 'Coordinates>) (second: Line2D<'Units, 'Coordinates>) : bool =
    match direction first, direction second with
    | Some d1, Some d2 -> d1 = d2 || Direction2D.reverse d1 = d2
    | _ -> false

let arePerpendicular (first: Line2D<'Units, 'Coordinates>) (second: Line2D<'Units, 'Coordinates>) =
    match direction first, direction second with
    | Some d1, Some d2 ->
        let d2' = Direction2D.rotateClockwise d2
        d1 = d2' || Direction2D.reverse d1 = d2'

    | _ -> false

let intersect
    (first: Line2D<'Units, 'Coordinates>)
    (second: Line2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> option =
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
            Vector2D.cross (q - p) s
            / Vector2D.cross r s

        p + (t * r) |> Some
