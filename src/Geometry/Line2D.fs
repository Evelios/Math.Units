namespace Geometry

open System
open Geometry

[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type Line2D =
    private
        { start: Point2D
          finish: Point2D }

    member this.Start = this.start
    member this.Finish = this.finish

    interface IComparable<Line2D> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Line2D as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Line2D) =
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
        | :? Line2D as other ->
            (this.start = other.start
             && this.finish = other.finish)
            || (this.start = other.finish
                && this.finish = other.start)
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.start, this.finish)

module Line2D =
    (* Builders *)
    let through (start: Point2D) (finish: Point2D) = { start = start; finish = finish }

    /// Create a line starting at point in a particular direction and length
    let fromPointAndVector (start: Point2D) (direction: Vector2D) =
        { start = start
          finish = start + direction }

    let private toLineSegment (line: Line2D) : LineSegment2D =
        LineSegment2D.from line.start line.finish

    (* Attributes *)

    let direction (line: Line2D) : Vector2D =
        Vector2D.normalize (line.start - line.finish)

    let length (line: Line2D) : float =
        Point2D.distanceTo line.start line.finish


    (* Modifiers *)

    let round (l: Line2D) =
        through (Point2D.round l.Start) (Point2D.round l.Finish)


    (* Queries *)

    let pointClosestTo (point: Point2D) (line: Line2D) : Point2D =
        let v : Vector2D = line.start |> Point2D.vectorTo point
        let lineDirection = direction line

        let alongVector =
            Vector2D.dotProduct v (direction line)
            * lineDirection

        line.start + alongVector


    let distanceToPoint (point: Point2D) (line: Line2D) : float =
        if line.start = point || line.finish = point then
            0.
        else
            Point2D.distanceTo point (pointClosestTo point line)

    let atPointInDirection (point: Point2D) (direction: Vector2D) : Line2D = through point (point + direction)

    let perpThroughPoint (point: Point2D) (line: Line2D) : Line2D =
        atPointInDirection point (Vector2D.rotateBy (Angle.inDegrees 90.<deg>) (direction line))

    let isPointOnLine (point: Point2D) (line: Line2D) =
        point = line.start
        || point = line.finish
        || point = pointClosestTo point line

    let areParallel (first: Line2D) (second: Line2D) : bool =
        let d1 = direction first
        let d2 = direction second

        d1 = d2 || Vector2D.neg d1 = d2

    let arePerpendicular (first: Line2D) (second: Line2D) =
        let d1 = (direction first)

        let d2 =
            Vector2D.rotateBy (Angle.pi / 2.) (direction second)

        d1 = d2 || Vector2D.neg d1 = d2

    let intersect (first: Line2D) (second: Line2D) : Point2D option =
        if areParallel first second then
            None
        else
            // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
            let p = first.start
            let q = second.start

            let r =
                first.start |> Point2D.vectorTo first.finish

            let s =
                second.start |> Point2D.vectorTo second.finish

            let t =
                Vector2D.crossProduct (q - p) s
                / Vector2D.crossProduct r s

            p + (t * r) |> Some
