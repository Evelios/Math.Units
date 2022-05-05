namespace Geometry

open System
open Geometry

// ---- Lengths ----

/// A finite, closed interval with a minimum and maximum number. This can
/// represent an interval of any type.
///
/// For example...
///     Interval float
///     Interval int
///     Interval Angle
[<CustomEquality>]
[<NoComparison>]
type Interval<'T when 'T: equality> =
    | Interval of 'T * 'T

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Interval<'T>) as other ->
            match this, other with
            | Interval (thisStart, thisFinish), Interval (otherStart, otherFinish) ->
                thisStart = otherStart && thisFinish = otherFinish

        | _ -> false

    override this.GetHashCode() : int =
        match this with
        | Interval (start, finish) -> HashCode.Combine(start, finish)

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
[<Struct>]
type Angle =
    | Radians of float

    // Accessors

    /// Convert an arbitrary angle to the equivalent angle in the range -180 to 180
    /// degrees (-π to π radians), by adding or subtracting some multiple of 360
    /// degrees (2π radians) if necessary.
    static member normalize(Radians r) =
        let twoPi = 2. * Math.PI
        let turns = float (int (r / twoPi))
        let radians = r - twoPi * turns

        if (radians > Math.PI) then
            Radians(radians - twoPi)
        else if (radians < -Math.PI) then
            Radians(twoPi + radians)
        else
            Radians radians

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

    member this.Equals(other: Angle) : bool =
        match Angle.normalize this, Angle.normalize other with
        | Radians lhs, Radians rhs -> almostEqual lhs rhs

    override this.GetHashCode() =
        match Angle.normalize this with
        | Radians radians ->
            (roundFloatTo Float.DigitPrecision radians)
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
    static member (*)(Length length: Length<'Unit>, Angle.Radians arc: Angle) : Length<'Unit> = Length(length * arc)
    static member (*)(Angle.Radians arc: Angle, Length length: Length<'Unit>) : Length<'Unit> = Length(arc * length)
    static member (*)(Length lhs: Length<'Unit>, Length rhs: Length<'Unit>) : Length<'Unit * 'Unit> = Length(lhs * rhs)
    static member (/)(Length length: Length<'Unit>, scale: float) : Length<'Unit> = Length(length / scale)
    static member (/)(Length length: Length<'Unit>, Length scale: Length<'Unit>) : float = length / scale
    static member (/)(Length length: Length<'Unit>, Angle.Radians arc: Angle) : Length<'Unit> = Length(length / arc)

    // Operator overloads through functions

    static member Pow(Length length: Length<'Unit>, power: float) : Length<'Unit> = Length(length ** power)


[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Size2D<'Unit> =
    { Width: Length<'Unit>
      Height: Length<'Unit> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Size2D<'Unit>) as other ->
            this.Width = other.Width
            && this.Height = other.Height

        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Width, this.Height)


// ---- Geometry ----



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

type Frame2D<'Unit, 'Coordinates, 'Defines> =
    { Origin: Point2D<'Unit, 'Coordinates>
      XDirection: Direction2D<'Coordinates>
      YDirection: Direction2D<'Coordinates> }


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

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Triangle2D<'Unit, 'Coordinates> =
    { P1: Point2D<'Unit, 'Coordinates>
      P2: Point2D<'Unit, 'Coordinates>
      P3: Point2D<'Unit, 'Coordinates> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Triangle2D<'Unit, 'Coordinates> as other ->
            this.P1 = other.P1
            && this.P2 = other.P2
            && this.P3 = other.P3
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.P1, this.P2, this.P3)

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type BoundingBox2D<'Unit, 'Coordinates> =
    { MinX: Length<'Unit>
      MaxX: Length<'Unit>
      MinY: Length<'Unit>
      MaxY: Length<'Unit> }

    member this.TopLeft : Point2D<'Unit, 'Coordinates> = { X = this.MinX; Y = this.MaxY }
    member this.TopRight : Point2D<'Unit, 'Coordinates> = { X = this.MaxX; Y = this.MaxY }
    member this.BottomRight : Point2D<'Unit, 'Coordinates> = { X = this.MaxX; Y = this.MinY }
    member this.BottomLeft : Point2D<'Unit, 'Coordinates> = { X = this.MinX; Y = this.MinY }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? BoundingBox2D<'Unit, 'Coordinates> as other ->
            this.MinX = other.MinX
            && this.MaxX = other.MaxX
            && this.MinY = other.MinY
            && this.MaxY = other.MaxY

        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.MinX, this.MaxX, this.MinY, this.MaxY)

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Rectangle2D<'Unit, 'Coordinates> =
    { Axes: Frame2D<'Unit, 'Coordinates, unit>
      Dimensions: Size2D<'Unit> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Rectangle2D<'Unit, 'Coordinates> as other ->
            this.Axes = other.Axes
            && this.Dimensions = other.Dimensions
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Axes, this.Dimensions)

[<CustomEquality>]
[<NoComparison>]
type Circle2D<'Unit, 'Coordinates> =
    { Center: Point2D<'Unit, 'Coordinates>
      Radius: Length<'Unit> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Circle2D<'Unit, 'Coordinates> as other ->
            this.Center = other.Center
            && this.Radius = other.Radius
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Center, this.Radius)

[<CustomEquality>]
[<NoComparison>]
type Ellipse2D<'Unit, 'Coordinates> =
    { Axes: Frame2D<'Unit, 'Coordinates, unit>
      XRadius: Length<'Unit>
      YRadius: Length<'Unit> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Ellipse2D<'Unit, 'Coordinates> as other ->
            this.Axes = other.Axes
            && this.XRadius = other.XRadius
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Axes, this.XRadius, this.YRadius)

type SweptAngle =
    | SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative

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

type Nondegenerate<'Unit, 'Coordinates> = Arc2D<'Unit, 'Coordinates>

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Polygon2D<'Unit, 'Coordinates> =
    { OuterLoop: Point2D<'Unit, 'Coordinates> list
      InnerLoops: Point2D<'Unit, 'Coordinates> list list }

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

    member this.LessThan(other: Polygon2D<'Unit, 'Coordinates>) =
        this.OuterLoop < other.OuterLoop
        && this.InnerLoops < other.InnerLoops

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Polygon2D<'Unit, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Polygon2D<'Unit, 'Coordinates>) : bool =
        this.OuterLoop = other.OuterLoop
        && this.InnerLoops = this.InnerLoops

    override this.GetHashCode() =
        HashCode.Combine(hash this.OuterLoop, hash this.InnerLoops)


type Polyline2D<'Unit, 'Coordinates> = Polyline2D of Point2D<'Unit, 'Coordinates> list
