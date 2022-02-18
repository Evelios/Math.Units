[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.LineSegment2D

(* Builders *)

/// Generate a line segment from two points. This doesn't perform any checks ensuring that the points are not equal.
/// If that is the behavior that you want you should use <see cref="safeFrom"/> function.
let from
    (start: Point2D<'Unit, 'Coordinates>)
    (finish: Point2D<'Unit, 'Coordinates>)
    : LineSegment2D<'Unit, 'Coordinates> =
    { Start = start; Finish = finish }

/// Safely create a line segment. This function returns `None` when the two points are almost equal.
/// This has to do with the <see cref="Geometry.Internal.Tolerance"/>.
let safeFrom (start: Point2D<'Unit, 'Coordinates>) (finish: Point2D<'Unit, 'Coordinates>) =
    if start = finish then
        None
    else
        Some(from start finish)

/// Create a line segment starting at point in a particular direction and length
let fromPointAndVector (start: Point2D<'Unit, 'Coordinates>) (direction: Vector2D<'Unit, 'Coordinates>) =
    { Start = start
      Finish = start + direction }


(* Attributes *)

let direction (line: LineSegment2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
    Vector2D.normalize (line.Finish - line.Start)

let length (line: LineSegment2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Point2D.distanceTo line.Start line.Finish


(* Modifiers *)

let round (l: LineSegment2D<'Unit, 'Coordinates>) =
    from (Point2D.round l.Start) (Point2D.round l.Finish)


(* Queries *)

let areParallel (first: LineSegment2D<'Unit, 'Coordinates>) (second: LineSegment2D<'Unit, 'Coordinates>) : bool =
    let d1 = direction first
    let d2 = direction second

    d1 = d2 || Vector2D.neg d1 = d2

let pointClosestTo
    (point: Point2D<'Unit, 'Coordinates>)
    (line: LineSegment2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =
    if point = line.Start || point = line.Finish then
        point
    else
        let v = line.Start |> Point2D.vectorTo point
        let lineLength = length line
        let lineDirection = direction line

        let dotProduct: Length<'Unit * 'Unit> =
            match Vector2D.dotProduct v lineDirection with
            | dotProduct when dotProduct < Length.zero -> Length.zero
            | dotProduct when dotProduct.value () > lineLength.value () ->
                Length.create<'Unit * 'Unit> (lineLength.value ())
            | dotProduct -> dotProduct

        let alongVector = dotProduct.value () * lineDirection

        printfn $"v: {v}"
        printfn $"lineLength: {lineLength}"
        printfn $"lineDirection: {lineDirection}"
        printfn $"dotProduct: {dotProduct}"

        line.Start + alongVector

let isPointOnLine (point: Point2D<'Unit, 'Coordinates>) (line: LineSegment2D<'Unit, 'Coordinates>) : bool =
    point = line.Start
    || point = line.Finish
    || point = pointClosestTo point line

let distanceToPoint (point: Point2D<'Unit, 'Coordinates>) (line: LineSegment2D<'Unit, 'Coordinates>) : Length<'Unit> =
    Point2D.distanceTo point (pointClosestTo point line)


/// Try to find the intersection between two lines. If the lines are parallel (even if they are overlapping) then no
/// intersection is returned
let intersect
    (lhs: LineSegment2D<'Unit, 'Coordinates>)
    (rhs: LineSegment2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> option =
    if areParallel lhs rhs then
        None
    else
        // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
        let p = lhs.Start
        let q = rhs.Start
        let r = lhs.Finish - lhs.Start
        let s = rhs.Finish - rhs.Start

        let t =
            Vector2D.crossProduct (q - p) s
            / Vector2D.crossProduct r s

        let u =
            Vector2D.crossProduct (p - q) r
            / Vector2D.crossProduct s r

        if (0.0 <= t && t <= 1.0) && (0.0 <= u && u <= 1.0) then
            p + (t * r) |> Some
        else
            None
