namespace Geometry

open System
open Geometry

[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type LineSegment2D =
    private
        { start: Point2D
          finish: Point2D }

    member this.Start = this.start
    member this.Finish = this.finish

    interface IComparable<LineSegment2D> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? LineSegment2D as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: LineSegment2D) =
        let firstLower = min this.start this.finish

        let firstGreater = max this.start this.finish

        let secondLower = min other.start other.finish

        let secondGreater = max other.start other.finish

        if firstLower = secondLower then
            firstGreater < secondGreater
        else
            firstLower < secondLower

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? LineSegment2D as other ->
            (this.start = other.start
             && this.finish = other.finish)
            || (this.start = other.finish
                && this.finish = other.start)
        | _ -> false

    static member (*)(lhs: LineSegment2D, rhs: float) : LineSegment2D =
        { start = lhs.start * rhs
          finish = lhs.finish * rhs }


    static member (*)(lhs: float, rhs: LineSegment2D) : LineSegment2D = rhs * lhs

    static member (/)(lhs: LineSegment2D, rhs: float) : LineSegment2D =
        { start = lhs.start / rhs
          finish = lhs.finish / rhs }


    static member (/)(lhs: float, rhs: LineSegment2D) : LineSegment2D = rhs / lhs

    override this.GetHashCode() : int =
        HashCode.Combine(this.start, this.finish)

module LineSegment2D =
    (* Builders *)

    /// Generate a line segment from two points. This doesn't perform any checks ensuring that the points are not equal.
    /// If that is the behavior that you want you should use <see cref="safeFrom"/> function.
    let from (start: Point2D) (finish: Point2D) = { start = start; finish = finish }

    /// Safely create a line segment. This function returns `None` when the two points are almost equal.
    /// This has to do with the <see cref="Geometry.Internal.Tolerance"/>.
    let safeFrom (start: Point2D) (finish: Point2D) =
        if start = finish then
            None
        else
            Some(from start finish)

    /// Create a line segment starting at point in a particular direction and length
    let fromPointAndVector (start: Point2D) (direction: Vector2D) =
        { start = start
          finish = start + direction }


    (* Attributes *)

    let direction (line: LineSegment2D) : Vector2D =
        Vector2D.normalize (line.finish - line.start)

    let length (line: LineSegment2D) : float =
        Point2D.distanceTo line.start line.finish

    
    (* Modifiers *)
    
    let round (l: LineSegment2D) = from (Point2D.round l.Start) (Point2D.round l.Finish)
    

    (* Queries *)

    let areParallel (first: LineSegment2D) (second: LineSegment2D) : bool =
        let d1 = direction first
        let d2 = direction second

        d1 = d2 || Vector2D.neg d1 = d2

    let pointClosestTo (point: Point2D) (line: LineSegment2D) =
        if point = line.start || point = line.finish then
            point
        else
            let v = line.start |> Point2D.vectorTo point
            let lineLength = length line

            let dotProduct : float =
                match Vector2D.dotProduct v (direction line) with
                | dotProduct when dotProduct < 0. -> 0.
                | dotProduct when dotProduct > lineLength -> lineLength
                | dotProduct -> dotProduct

            let alongVector = dotProduct * direction line
            line.start + alongVector

    let isPointOnLine (point: Point2D) (line: LineSegment2D) : bool =
        point = line.start
        || point = line.finish
        || point = pointClosestTo point line

    let distanceToPoint (point: Point2D) (line: LineSegment2D) : float =
        Point2D.distanceTo point (pointClosestTo point line)


    /// Try to find the intersection between two lines. If the lines are parallel (even if they are overlapping) then no
    /// intersection is returned
    let intersect (lhs: LineSegment2D) (rhs: LineSegment2D) : Point2D option =
        if areParallel lhs rhs then
            None
        else
            // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
            let p = lhs.start
            let q = rhs.start
            let r = lhs.finish - lhs.start
            let s = rhs.finish - rhs.start

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
