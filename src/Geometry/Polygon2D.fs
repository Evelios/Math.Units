namespace Geometry

open System

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Polygon2D<'Length, 'Coordinates> =
    { Points: Point2D<'Length, 'Coordinates> list }

    (* Comparable interfaces *)

    interface IComparable<Polygon2D<'Length, 'Coordinates>> with
        member this.CompareTo(polygon) = this.Comparison(polygon)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Polygon2D<'Length, 'Coordinates> as polygon -> this.Comparison(polygon)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    // TODO
    member this.LessThan(other: Polygon2D<'Length, 'Coordinates>) = this.Points < other.Points

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Polygon2D<'Length, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Polygon2D<'Length, 'Coordinates>) : bool = this.Points = other.Points

    override this.GetHashCode() = this.Points.GetHashCode()

module Polygon2D =
    (* Builders *)

    /// Points that are each connected together. Polygon's have the assumption that the last point is connected back to
    /// the first point, so it is not necessary to explicitly add it.
    let singleLoop (points: Point2D<'Length, 'Coordinates> list) : Polygon2D<'Length, 'Coordinates> = { Points = points }

    (* Accessors *)

    let boundingBox (polygon: Polygon2D<'Length, 'Coordinates>) : BoundingBox2D<'Length, 'Coordinates> =
        BoundingBox2D.containingPoints polygon.Points (BoundingBox2D.empty ())

    (* Modifiers *)

    let translate (amount: Vector2D<'Length, 'Coordinates>) (polygon: Polygon2D<'Length, 'Coordinates>) : Polygon2D<'Length, 'Coordinates> =
        { Points = List.map (Point2D.translate amount) polygon.Points }


    let rotateAround (reference: Point2D<'Length, 'Coordinates>) (angle: Angle) (polygon: Polygon2D<'Length, 'Coordinates>) : Polygon2D<'Length, 'Coordinates> =
        { Points = List.map (Point2D.rotateAround reference angle) polygon.Points }

    let placeIn (frame: Frame2D<'Length, 'Coordinates>) (polygon: Polygon2D<'Length, 'Coordinates>) : Polygon2D<'Length, 'Coordinates> =
        { Points = List.map (Point2D.placeIn frame) polygon.Points }
