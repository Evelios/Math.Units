namespace Geometry

open System
open Geometry

// ---- Lengths ----

type Unitless = Unitless

type Pixels = Pixels

type Meters = Meters

/// A percentage value. The default range for percentages is 0 to 1 but can also be given in the range 0 to 100.
[<CustomEquality>]
[<CustomComparison>]
type Percent =
    | Percent of float

    interface IComparable<Percent> with
        member this.CompareTo(percent) = this.Comparison(percent)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Percent as percent -> this.Comparison(percent)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Percent other: Percent) =
        match this with
        | Percent self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Percent as other -> this.Equals(other)
        | _ -> false

    member this.Equals(Percent other: Percent) : bool =
        match this with
        | Percent self -> almostEqual self other

    override this.GetHashCode() =
        match this with
        | Percent self ->
            (roundFloatTo Float.DigitPrecision self)
                .GetHashCode()


    // Operators

    static member (+)(Percent lhs: Percent, Percent rhs: Percent) : Percent = Percent(lhs + rhs)
    static member (-)(Percent lhs: Percent, Percent rhs: Percent) : Percent = Percent(lhs - rhs)
    static member (*)(Percent percent: Percent, scale: float) : Percent = Percent(percent * scale)
    static member (*)(Percent percent: Percent, Percent scale: Percent) : Percent = Percent(percent * scale)
    static member (/)(Percent percent: Percent, scale: float) : Percent = Percent(percent / scale)
    static member (/)(Percent percent: Percent, Percent scale: Percent) : Percent = Percent(percent / scale)


[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Length<'Unit> =
    | Length of float

    // Accessors
    static member create a = Length a

    interface IComparable<Length<'Unit>> with
        member this.CompareTo(length) = this.Comparison(length)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? (Length<'Unit>) as length -> this.Comparison(length)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Length other: Length<'Unit>) =
        match this with
        | Length self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Length<'Unit>) as other -> this.Equals(other)
        | _ -> false

    member this.Equals(Length other: Length<'Unit>) : bool =
        match this with
        | Length self -> almostEqual self other

    override this.GetHashCode() =
        match this with
        | Length self ->
            (roundFloatTo Float.DigitPrecision self)
                .GetHashCode()

    // Unitless Operations
    static member (*)(Length length: Length<'Unit>, Length scale: Length<Unitless>) : Length<'Unit> =
        Length(length * scale)

    static member (*)(Length scale: Length<Unitless>, Length length: Length<'Unit>) : Length<'Unit> =
        Length(scale * length)

    // Generic Operations

    static member (+)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit> = Length(lhs + rhs)
    static member (-)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit> = Length(lhs - rhs)
    static member (~-)(Length length: Length<'Unit>) : Length<'Unit> = Length(-length)
    static member (*)(Length length: Length<'Unit>, scale: float) : Length<'Unit> = Length(length * scale)
    static member (*)(scale: float, Length length: Length<'Unit>) : Length<'Unit> = Length(scale * length)
    static member (*)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit * 'Unit> = Length(lhs * rhs)
    static member (/)(Length length: Length<'Unit>, scale: float) : Length<'Unit> = Length(length / scale)
    static member (/)(Length length: Length<'Unit>, Length scale: Length<'Unit>) : float = length / scale

    // Operator overloads through functions

    static member Pow(Length length: Length<'Unit>, power: float) : Length<'Unit> = Length(length ** power)


type Size<'Unit> =
    { Width: Length<'Unit>
      Height: Length<'Unit> }


// ---- Geometry ----

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Angle =
    | Radians of float

    // Accessors
    static member create a = Radians a
    static member value(Radians a) = a

    member this.value() : float = Angle.value this

    interface IComparable<Angle> with
        member this.CompareTo(angle) = this.Comparison(angle)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Angle as angle -> this.Comparison(angle)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(Radians other: Angle) =
        match this with
        | Radians self -> self < other


    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Angle as other -> this.Equals(other)
        | _ -> false

    member this.Equals(Radians other: Angle) : bool =
        match this with
        | Radians self -> almostEqual (self % 2. * Math.PI) (other % 2. * Math.PI)

    override this.GetHashCode() =
        match this with
        | Radians self ->
            (roundFloatTo Float.DigitPrecision (self % 2. * Math.PI))
                .GetHashCode()


    // Math Operators

    static member (+)(Radians lhs: Angle, Radians rhs: Angle) : Angle = Radians(lhs + rhs)
    static member (-)(Radians lhs: Angle, Radians rhs: Angle) : Angle = Radians(lhs - rhs)
    static member (~-)(Radians angle: Angle) : Angle = Radians -angle
    static member (*)(Radians lhs: Angle, rhs: float) : Angle = Radians(lhs * rhs)
    static member (*)(lhs: float, Radians rhs: Angle) : Angle = Radians(rhs * lhs)
    static member (/)(Radians lhs: Angle, rhs: float) : Angle = Radians(lhs / rhs)
    static member (/)(Radians lhs: Angle, Radians rhs: Angle) : float = lhs / rhs


[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Direction2D<'Coordinates> =
    { X: float
      Y: float }

    // Comparable interfaces

    interface IComparable<Direction2D<'Coordinates>> with
        member this.CompareTo(direction) = this.Comparison(direction)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? (Direction2D<'Coordinates>) as direction -> this.Comparison(direction)
            | _ -> failwith "incompatible comparison"

    static member xy (x: float) (y: float) : Direction2D<'Coordinates> option =
        if x = 0. && y = 0. then
            None
        else
            let magnitude = sqrt ((x * x) + (y * y))

            Some
                { Direction2D.X = x / magnitude
                  Direction2D.Y = y / magnitude }

    static member xyLength
        (Length.Length x: Length<'Unit>)
        (Length.Length y: Length<'Unit>)
        : Direction2D<'Coordinates> option =
        Direction2D.xy x y

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Direction2D<'Coordinates>) =
        if almostEqual this.X other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Direction2D<'Coordinates>) as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Direction2D<'Coordinates>) : bool =
        almostEqual this.X other.X
        && almostEqual this.Y other.Y

    override this.GetHashCode() =
        HashCode.Combine((roundFloatTo Float.DigitPrecision this.X), (roundFloatTo Float.DigitPrecision this.Y))


[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Vector2D<'Unit, 'Coordinates> =
    { X: Length<'Unit>
      Y: Length<'Unit> }

    // Comparable interfaces

    interface IComparable<Vector2D<'Unit, 'Coordinates>> with
        member this.CompareTo(vector) = this.Comparison(vector)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vector2D<'Unit, 'Coordinates> as vector -> this.Comparison(vector)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Vector2D<'Unit, 'Coordinates>) =
        if this.X = other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector2D<'Unit, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Vector2D<'Unit, 'Coordinates>) : bool = this.X = other.X && this.Y = other.Y

    override this.GetHashCode() = HashCode.Combine(this.X, this.Y)

    static member (+)
        (
            lhs: Vector2D<'Unit, 'Coordinates>,
            rhs: Vector2D<'Unit, 'Coordinates>
        ) : Vector2D<'Unit, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (-)
        (
            lhs: Vector2D<'Unit, 'Coordinates>,
            rhs: Vector2D<'Unit, 'Coordinates>
        ) : Vector2D<'Unit, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (~-)(vector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
        { X = vector.X; Y = vector.Y }

    static member (*)(vector: Vector2D<'Unit, 'Coordinates>, scale: float) : Vector2D<'Unit, 'Coordinates> =
        { X = vector.X * scale
          Y = vector.Y * scale }

    static member (*)(scale: float, vector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
        vector * scale

    static member (/)(vector: Vector2D<'Unit, 'Coordinates>, scale: float) : Vector2D<'Unit, 'Coordinates> =
        { X = vector.X / scale
          Y = vector.Y / scale }

    static member (/)(scale: float, vector: Vector2D<'Unit, 'Coordinates>) : Vector2D<'Unit, 'Coordinates> =
        vector / scale

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Point2D<'Unit, 'Coordinates> =
    { X: Length<'Unit>
      Y: Length<'Unit> }

    // Comparable interfaces

    interface IComparable<Point2D<'Unit, 'Coordinates>> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D<'Unit, 'Coordinates> as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Point2D<'Unit, 'Coordinates>) =
        if this.X = other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D<'Unit, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Point2D<'Unit, 'Coordinates>) : bool = this.X = other.X && this.Y = other.Y

    override this.GetHashCode() = HashCode.Combine(this.X, this.Y)

    static member (+)
        (
            lhs: Point2D<'Unit, 'Coordinates>,
            rhs: Vector2D<'Unit, 'Coordinates>
        ) : Point2D<'Unit, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (-)
        (
            lhs: Point2D<'Unit, 'Coordinates>,
            rhs: Point2D<'Unit, 'Coordinates>
        ) : Vector2D<'Unit, 'Coordinates> =
        { X = (lhs.X - rhs.X)
          Y = (lhs.Y - rhs.Y) }

    static member (-)
        (
            lhs: Point2D<'Unit, 'Coordinates>,
            rhs: Vector2D<'Unit, 'Coordinates>
        ) : Point2D<'Unit, 'Coordinates> =
        { X = (lhs.X - rhs.X)
          Y = (lhs.Y - rhs.Y) }

    static member (~-)(point: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
        { X = -point.X; Y = -point.Y }

    static member (*)(lhs: Point2D<'Unit, 'Coordinates>, rhs: float) : Point2D<'Unit, 'Coordinates> =
        { X = lhs.X * rhs; Y = lhs.Y * rhs }

    static member (*)(lhs: float, rhs: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = rhs * lhs

    static member (/)(lhs: Point2D<'Unit, 'Coordinates>, rhs: float) : Point2D<'Unit, 'Coordinates> =
        { X = lhs.X / rhs; Y = lhs.Y / rhs }

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Axis2D<'Unit, 'Coordinates> =
    { Origin: Point2D<'Unit, 'Coordinates>
      Direction: Direction2D<'Coordinates> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Axis2D<'Unit, 'Coordinates> as other ->
            this.Origin = other.Origin
            && this.Direction = other.Direction

        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Origin, this.Direction)


[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type LineSegment2D<'Unit, 'Coordinates> =
    { Start: Point2D<'Unit, 'Coordinates>
      Finish: Point2D<'Unit, 'Coordinates> }

    interface IComparable<LineSegment2D<'Unit, 'Coordinates>> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? LineSegment2D<'Unit, 'Coordinates> as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: LineSegment2D<'Unit, 'Coordinates>) =
        let firstLower = min this.Start this.Finish

        let firstGreater = max this.Start this.Finish

        let secondLower = min other.Start other.Finish

        let secondGreater = max other.Start other.Finish

        if firstLower = secondLower then
            firstGreater < secondGreater
        else
            firstLower < secondLower

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? LineSegment2D<'Unit, 'Coordinates> as other ->
            (this.Start = other.Start
             && this.Finish = other.Finish)
            || (this.Start = other.Finish
                && this.Finish = other.Start)
        | _ -> false

    static member (*)(lhs: LineSegment2D<'Unit, 'Coordinates>, rhs: float) : LineSegment2D<'Unit, 'Coordinates> =
        { Start = lhs.Start * rhs
          Finish = lhs.Finish * rhs }


    static member (*)(lhs: float, rhs: LineSegment2D<'Unit, 'Coordinates>) : LineSegment2D<'Unit, 'Coordinates> =
        rhs * lhs

    static member (/)(lhs: LineSegment2D<'Unit, 'Coordinates>, rhs: float) : LineSegment2D<'Unit, 'Coordinates> =
        { Start = lhs.Start / rhs
          Finish = lhs.Finish / rhs }


    static member (/)(lhs: float, rhs: LineSegment2D<'Unit, 'Coordinates>) : LineSegment2D<'Unit, 'Coordinates> =
        rhs / lhs

    override this.GetHashCode() : int =
        HashCode.Combine(this.Start, this.Finish)

[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type Line2D<'Unit, 'Coordinates> =
    { Start: Point2D<'Unit, 'Coordinates>
      Finish: Point2D<'Unit, 'Coordinates> }


    interface IComparable<Line2D<'Unit, 'Coordinates>> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Line2D<'Unit, 'Coordinates> as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Line2D<'Unit, 'Coordinates>) =
        let firstLower = min this.Start this.Finish

        let firstGreater = max this.Start this.Finish

        let secondLower = min other.Start other.Finish

        let secondGreater = max other.Start other.Finish

        if firstLower = secondLower then
            firstGreater < secondGreater
        else
            firstLower < secondLower

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Line2D<'Unit, 'Coordinates> as other ->
            (this.Start = other.Start
             && this.Finish = other.Finish)
            || (this.Start = other.Finish
                && this.Finish = other.Start)
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Start, this.Finish)


[<Struct>]
type BoundingBox2D<'Unit, 'Coordinates> =
    { MinX: Length<'Unit>
      MaxX: Length<'Unit>
      MaxY: Length<'Unit>
      MinY: Length<'Unit> }

    member this.TopLeft : Point2D<'Unit, 'Coordinates> = { X = this.MinX; Y = this.MaxY }
    member this.TopRight : Point2D<'Unit, 'Coordinates> = { X = this.MaxX; Y = this.MaxY }
    member this.BottomRight : Point2D<'Unit, 'Coordinates> = { X = this.MaxX; Y = this.MinY }
    member this.BottomLeft : Point2D<'Unit, 'Coordinates> = { X = this.MinX; Y = this.MinY }

type Circle2D<'Unit, 'Coordinates> =
    { Center: Point2D<'Unit, 'Coordinates>
      Radius: Length<'Unit> }

[<CustomEquality>]
[<NoComparison>]
type Arc2D<'Unit, 'Coordinates> =
    { StartPoint: Point2D<'Unit, 'Coordinates>
      XDirection: Direction2D<'Coordinates>
      SignedLength: Length<'Unit>
      SweptAngle: Angle }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Arc2D<'Unit, 'Coordinates> as other ->
            this.StartPoint = other.StartPoint
            && this.XDirection = other.XDirection
            && this.SignedLength = other.SignedLength
            && this.SweptAngle = other.SweptAngle
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.StartPoint, this.XDirection, this.SignedLength, this.SweptAngle)

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Polygon2D<'Unit, 'Coordinates> =
    { Points: Point2D<'Unit, 'Coordinates> list }

    // Comparable interfaces

    interface IComparable<Polygon2D<'Unit, 'Coordinates>> with
        member this.CompareTo(polygon) = this.Comparison(polygon)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Polygon2D<'Unit, 'Coordinates> as polygon -> this.Comparison(polygon)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    // TODO
    member this.LessThan(other: Polygon2D<'Unit, 'Coordinates>) = this.Points < other.Points

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Polygon2D<'Unit, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Polygon2D<'Unit, 'Coordinates>) : bool = this.Points = other.Points

    override this.GetHashCode() = this.Points.GetHashCode()

type Frame2D<'Unit, 'Coordinates> =
    { Origin: Point2D<'Unit, 'Coordinates>
      XDirection: Direction2D<'Coordinates>
      YDirection: Direction2D<'Coordinates> }
