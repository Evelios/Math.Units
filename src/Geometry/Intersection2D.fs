namespace Geometry

module Intersection2D =
    open Utilities.Extensions

    /// Try to find the intersection between a line segment and a line. If the lines are parallel (even if they are
    /// overlapping) then no intersection is returned.
    let lineSegmentAndLine (first: LineSegment2D<'Length, 'Coordinates>) (second: Line2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> option =
        let areParallel =
            let d1 = LineSegment2D.direction first
            let d2 = Line2D.direction second
            d1 = d2 || Vector2D.neg d1 = d2

        if areParallel then
            None
        else
            // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
            let p = first.start
            let q = second.start
            let r = first.finish - first.start
            let s = second.finish - second.start

            let t =
                Vector2D.crossProduct (q - p) s
                / Vector2D.crossProduct r s

            if (0.0 <= t && t <= 1.0) then
                p + (t * r) |> Some
            else
                None

    let lineAndLineSegment line segment = lineSegmentAndLine segment line

    /// Get all the intersection points between a bounding box and a line
    let boundingBoxAndLine (bbox: BoundingBox2D<'Length, 'Coordinates>) (line: Line2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> list =
        BoundingBox2D.lineSegments bbox
        |> List.filterMap (lineAndLineSegment line)
        |> List.distinct
