namespace Geometry

open System

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Vector2D =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Vector2D> with
        member this.CompareTo(vector) = this.Comparison(vector)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vector2D as vector -> this.Comparison(vector)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Vector2D) =
        if almostEqual (float this.x) (float other.x) then
            float this.y < float other.y
        else
            float this.x < float other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector2D as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Vector2D) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (+)(lhs: Vector2D, rhs: Vector2D) : Vector2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (-)(lhs: Vector2D, rhs: Vector2D) : Vector2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (~-)(vector: Vector2D) : Vector2D = { x = vector.X; y = vector.Y }

    static member (*)(vector: Vector2D, scale: float) : Vector2D =
        { x = vector.x * scale
          y = vector.y * scale }

    static member (*)(scale: float, vector: Vector2D) : Vector2D = vector * scale

    static member (/)(vector: Vector2D, scale: float) : Vector2D =
        { x = vector.x / scale
          y = vector.y / scale }

    static member (/)(scale: float, vector: Vector2D) : Vector2D = vector / scale

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Point2D =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Point2D> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Point2D) =
        if almostEqual (float this.x) (float other.x) then
            float this.y < float other.y
        else
            float this.x < float other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Point2D) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (-)(lhs: Point2D, rhs: Point2D) : Vector2D =
        { x = (lhs.x - rhs.x)
          y = (lhs.y - rhs.y) }
        
    static member (-)(lhs: Point2D, rhs: Vector2D) : Vector2D =
        { x = (lhs.x - rhs.x)
          y = (lhs.y - rhs.y) }

    static member (~-)(point: Point2D) : Point2D = { x = -point.X; y = -point.Y }

    static member (+)(lhs: Point2D, rhs: Vector2D) : Point2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (*)(lhs: Point2D, rhs: float) : Point2D = { x = lhs.x * rhs; y = lhs.y * rhs }

    static member (*)(lhs: float, rhs: Point2D) : Point2D = rhs * lhs

    static member (/)(lhs: Point2D, rhs: float) : Point2D = { x = lhs.x / rhs; y = lhs.y / rhs }

    static member (/)(lhs: float, rhs: Point2D) : Point2D = rhs / lhs

[<RequireQualifiedAccess>]
type Direction2D = { X: float; Y: float }

type Frame2D =
    { Origin: Point2D
      XDirection: Direction2D
      YDirection: Direction2D }
