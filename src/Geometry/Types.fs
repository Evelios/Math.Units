namespace Geometry

open System

// ---- Lengths ----

type Pixels = Pixels

type Meters = Meters

type 'Unit Length = 'Unit

// ---- Geometry ----

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Vector2D<'Length, 'Coordinates> =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Vector2D<'Length, 'Coordinates>> with
        member this.CompareTo(vector) = this.Comparison(vector)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vector2D<'Length, 'Coordinates> as vector -> this.Comparison(vector)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Vector2D<'Length, 'Coordinates>) =
        if almostEqual (float this.x) (float other.x) then
            float this.y < float other.y
        else
            float this.x < float other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector2D<'Length, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Vector2D<'Length, 'Coordinates>) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (+)(lhs: Vector2D<'Length, 'Coordinates>, rhs: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (-)(lhs: Vector2D<'Length, 'Coordinates>, rhs: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (~-)(vector: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = { x = vector.X; y = vector.Y }

    static member (*)(vector: Vector2D<'Length, 'Coordinates>, scale: float) : Vector2D<'Length, 'Coordinates> =
        { x = vector.x * scale
          y = vector.y * scale }

    static member (*)(scale: float, vector: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = vector * scale

    static member (/)(vector: Vector2D<'Length, 'Coordinates>, scale: float) : Vector2D<'Length, 'Coordinates> =
        { x = vector.x / scale
          y = vector.y / scale }

    static member (/)(scale: float, vector: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = vector / scale

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Point2D<'Length, 'Coordinates> =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Point2D<'Length, 'Coordinates>> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D<'Length, 'Coordinates> as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Point2D<'Length, 'Coordinates>) =
        if almostEqual (float this.x) (float other.x) then
            this.y < other.y
        else
            this.x < other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D<'Length, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Point2D<'Length, 'Coordinates>) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (-)(lhs: Point2D<'Length, 'Coordinates>, rhs: Point2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        { x = (lhs.x - rhs.x)
          y = (lhs.y - rhs.y) }
        
    static member (-)(lhs: Point2D<'Length, 'Coordinates>, rhs: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        { x = (lhs.x - rhs.x)
          y = (lhs.y - rhs.y) }

    static member (~-)(point: Point2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> = { x = -point.X; y = -point.Y }

    static member (+)(lhs: Point2D<'Length, 'Coordinates>, rhs: Vector2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (*)(lhs: Point2D<'Length, 'Coordinates>, rhs: float) : Point2D<'Length, 'Coordinates> = { x = lhs.x * rhs; y = lhs.y * rhs }

    static member (*)(lhs: float, rhs: Point2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> = rhs * lhs

    static member (/)(lhs: Point2D<'Length, 'Coordinates>, rhs: float) : Point2D<'Length, 'Coordinates> = { x = lhs.x / rhs; y = lhs.y / rhs }

    static member (/)(lhs: float, rhs: Point2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> = rhs / lhs

[<RequireQualifiedAccess>]
type Direction2D = { X: float; Y: float }

type Frame2D<'Length, 'Coordinates> =
    { Origin: Point2D<'Length, 'Coordinates>
      XDirection: Direction2D
      YDirection: Direction2D }
