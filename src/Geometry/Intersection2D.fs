module Geometry.Intersection2D

open Utilities.Extensions

/// Try to find the intersection between a line segment and a line. If the lines are parallel (even if they are
/// overlapping) then no intersection is returned.
let lineSegmentAndLine (first: LineSegment2D<'Unit, 'Coordinates>) (second: Line2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> option =
    let areParallel =
        let d1 = LineSegment2D.direction first
        let d2 = Line2D.direction second
        d1 = d2 || Vector2D.neg d1 = d2

    if areParallel then
        None
    else
        // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
        let p = first.Start
        let q = second.Start
        let r = first.Finish - first.Start
        let s = second.Finish - second.Start

        let t =
            Vector2D.crossProduct (q - p) s
            / Vector2D.crossProduct r s

        if (0.0 <= t && t <= 1.0) then
            p + (t * r) |> Some
        else
            None

let lineAndLineSegment line segment = lineSegmentAndLine segment line

/// Get all the intersection points between a bounding box and a line
let boundingBoxAndLine (bbox: BoundingBox2D<'Unit, 'Coordinates>) (line: Line2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> list =
    BoundingBox2D.lineSegments bbox
    |> List.filterMap (lineAndLineSegment line)
    |> List.distinct
