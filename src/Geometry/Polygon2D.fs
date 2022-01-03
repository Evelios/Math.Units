namespace Geometry

open System

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Polygon2D =
    { Points: Point2D list }

    (* Comparable interfaces *)

    interface IComparable<Polygon2D> with
        member this.CompareTo(polygon) = this.Comparison(polygon)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Polygon2D as polygon -> this.Comparison(polygon)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    // TODO
    member this.LessThan(other: Polygon2D) = this.Points < other.Points

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Polygon2D as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Polygon2D) : bool = this.Points = other.Points

    override this.GetHashCode() = this.Points.GetHashCode()

module Polygon2D =
    (* Builders *)

    /// Points that are each connected together. Polygon's have the assumption that the last point is connected back to
    /// the first point, so it is not necessary to explicitly add it.
    let singleLoop (points: Point2D list) : Polygon2D = { Points = points }

    (* Accessors *)

    let boundingBox (polygon: Polygon2D) : BoundingBox2D =
        BoundingBox2D.containingPoints polygon.Points BoundingBox2D.empty

    (* Modifiers *)

    let translate (amount: Vector2D) (polygon: Polygon2D) : Polygon2D =
        { Points = List.map (Point2D.translate amount) polygon.Points }


    let rotateAround (reference: Point2D) (angle: Angle) (polygon: Polygon2D) : Polygon2D =
        { Points = List.map (Point2D.rotateAround reference angle) polygon.Points }

    let placeIn (frame: Frame2D) (polygon: Polygon2D) : Polygon2D =
        { Points = List.map (Point2D.placeIn frame) polygon.Points }
