namespace Geometry

open System

open Units

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Size2D<'Units> =
    { Width: Quantity<'Units>
      Height: Quantity<'Units> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Size2D<'Units>) as other ->
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


    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Direction2D<'Coordinates>) =
        if Float.almostEqual this.X other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Direction2D<'Coordinates>) as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Direction2D<'Coordinates>) : bool =
        Float.almostEqual this.X other.X
        && Float.almostEqual this.Y other.Y

    override this.GetHashCode() =
        HashCode.Combine((Float.roundFloatTo Float.DigitPrecision this.X), (Float.roundFloatTo Float.DigitPrecision this.Y))


[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Vector2D<'Units, 'Coordinates> =
    { X: Quantity<'Units>
      Y: Quantity<'Units> }

    // Comparable interfaces

    interface IComparable<Vector2D<'Units, 'Coordinates>> with
        member this.CompareTo(vector) = this.Comparison(vector)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vector2D<'Units, 'Coordinates> as vector -> this.Comparison(vector)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Vector2D<'Units, 'Coordinates>) =
        if this.X = other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector2D<'Units, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Vector2D<'Units, 'Coordinates>) : bool = this.X = other.X && this.Y = other.Y

    override this.GetHashCode() = HashCode.Combine(this.X, this.Y)

    static member (+)
        (
            lhs: Vector2D<'Units, 'Coordinates>,
            rhs: Vector2D<'Units, 'Coordinates>
        ) : Vector2D<'Units, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (-)
        (
            lhs: Vector2D<'Units, 'Coordinates>,
            rhs: Vector2D<'Units, 'Coordinates>
        ) : Vector2D<'Units, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (~-)(vector: Vector2D<'Units, 'Coordinates>) : Vector2D<'Units, 'Coordinates> =
        { X = vector.X; Y = vector.Y }

    static member (*)(vector: Vector2D<'Units, 'Coordinates>, scale: float) : Vector2D<'Units, 'Coordinates> =
        { X = vector.X * scale
          Y = vector.Y * scale }

    static member (*)(scale: float, vector: Vector2D<'Units, 'Coordinates>) : Vector2D<'Units, 'Coordinates> =
        vector * scale

    static member (/)(vector: Vector2D<'Units, 'Coordinates>, scale: float) : Vector2D<'Units, 'Coordinates> =
        { X = vector.X / scale
          Y = vector.Y / scale }

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Point2D<'Units, 'Coordinates> =
    { X: Quantity<'Units>
      Y: Quantity<'Units> }

    // Comparable interfaces

    interface IComparable<Point2D<'Units, 'Coordinates>> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D<'Units, 'Coordinates> as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Point2D<'Units, 'Coordinates>) =
        if this.X = other.X then
            this.Y < other.Y
        else
            this.X < other.X

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D<'Units, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Point2D<'Units, 'Coordinates>) : bool = this.X = other.X && this.Y = other.Y

    override this.GetHashCode() = HashCode.Combine(this.X, this.Y)

    static member (+)
        (
            lhs: Point2D<'Units, 'Coordinates>,
            rhs: Vector2D<'Units, 'Coordinates>
        ) : Point2D<'Units, 'Coordinates> =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (-)
        (
            lhs: Point2D<'Units, 'Coordinates>,
            rhs: Point2D<'Units, 'Coordinates>
        ) : Vector2D<'Units, 'Coordinates> =
        { X = (lhs.X - rhs.X)
          Y = (lhs.Y - rhs.Y) }

    static member (-)
        (
            lhs: Point2D<'Units, 'Coordinates>,
            rhs: Vector2D<'Units, 'Coordinates>
        ) : Point2D<'Units, 'Coordinates> =
        { X = (lhs.X - rhs.X)
          Y = (lhs.Y - rhs.Y) }

    static member (~-)(point: Point2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
        { X = -point.X; Y = -point.Y }

    static member (*)(lhs: Point2D<'Units, 'Coordinates>, rhs: float) : Point2D<'Units, 'Coordinates> =
        { X = lhs.X * rhs; Y = lhs.Y * rhs }

    static member (*)(lhs: float, rhs: Point2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> = rhs * lhs

    static member (/)(lhs: Point2D<'Units, 'Coordinates>, rhs: float) : Point2D<'Units, 'Coordinates> =
        { X = lhs.X / rhs; Y = lhs.Y / rhs }

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type Axis2D<'Units, 'Coordinates> =
    { Origin: Point2D<'Units, 'Coordinates>
      Direction: Direction2D<'Coordinates> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Axis2D<'Units, 'Coordinates> as other ->
            this.Origin = other.Origin
            && this.Direction = other.Direction

        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Origin, this.Direction)

type Frame2D<'Units, 'Coordinates, 'Defines> =
    { Origin: Point2D<'Units, 'Coordinates>
      XDirection: Direction2D<'Coordinates>
      YDirection: Direction2D<'Coordinates> }


[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type LineSegment2D<'Units, 'Coordinates> =
    { Start: Point2D<'Units, 'Coordinates>
      Finish: Point2D<'Units, 'Coordinates> }

    interface IComparable<LineSegment2D<'Units, 'Coordinates>> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? LineSegment2D<'Units, 'Coordinates> as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: LineSegment2D<'Units, 'Coordinates>) =
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
        | :? LineSegment2D<'Units, 'Coordinates> as other ->
            (this.Start = other.Start
             && this.Finish = other.Finish)
            || (this.Start = other.Finish
                && this.Finish = other.Start)
        | _ -> false

    static member (*)(lhs: LineSegment2D<'Units, 'Coordinates>, rhs: float) : LineSegment2D<'Units, 'Coordinates> =
        { Start = lhs.Start * rhs
          Finish = lhs.Finish * rhs }


    static member (*)(lhs: float, rhs: LineSegment2D<'Units, 'Coordinates>) : LineSegment2D<'Units, 'Coordinates> =
        rhs * lhs

    static member (/)(lhs: LineSegment2D<'Units, 'Coordinates>, rhs: float) : LineSegment2D<'Units, 'Coordinates> =
        { Start = lhs.Start / rhs
          Finish = lhs.Finish / rhs }


    static member (/)(lhs: float, rhs: LineSegment2D<'Units, 'Coordinates>) : LineSegment2D<'Units, 'Coordinates> =
        rhs / lhs

    override this.GetHashCode() : int =
        HashCode.Combine(this.Start, this.Finish)

[<CustomEquality>]
[<CustomComparison>]
[<Struct>]
type Line2D<'Units, 'Coordinates> =
    { Start: Point2D<'Units, 'Coordinates>
      Finish: Point2D<'Units, 'Coordinates> }


    interface IComparable<Line2D<'Units, 'Coordinates>> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Line2D<'Units, 'Coordinates> as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Line2D<'Units, 'Coordinates>) =
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
        | :? Line2D<'Units, 'Coordinates> as other ->
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
type Triangle2D<'Units, 'Coordinates> =
    { P1: Point2D<'Units, 'Coordinates>
      P2: Point2D<'Units, 'Coordinates>
      P3: Point2D<'Units, 'Coordinates> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Triangle2D<'Units, 'Coordinates> as other ->
            this.P1 = other.P1
            && this.P2 = other.P2
            && this.P3 = other.P3
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.P1, this.P2, this.P3)

[<CustomEquality>]
[<NoComparison>]
[<Struct>]
type BoundingBox2D<'Units, 'Coordinates> =
    { MinX: Quantity<'Units>
      MaxX: Quantity<'Units>
      MinY: Quantity<'Units>
      MaxY: Quantity<'Units> }

    member this.TopLeft : Point2D<'Units, 'Coordinates> = { X = this.MinX; Y = this.MaxY }
    member this.TopRight : Point2D<'Units, 'Coordinates> = { X = this.MaxX; Y = this.MaxY }
    member this.BottomRight : Point2D<'Units, 'Coordinates> = { X = this.MaxX; Y = this.MinY }
    member this.BottomLeft : Point2D<'Units, 'Coordinates> = { X = this.MinX; Y = this.MinY }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? BoundingBox2D<'Units, 'Coordinates> as other ->
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
type Rectangle2D<'Units, 'Coordinates> =
    { Axes: Frame2D<'Units, 'Coordinates, unit>
      Dimensions: Size2D<'Units> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Rectangle2D<'Units, 'Coordinates> as other ->
            this.Axes = other.Axes
            && this.Dimensions = other.Dimensions
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Axes, this.Dimensions)

[<CustomEquality>]
[<NoComparison>]
type Circle2D<'Units, 'Coordinates> =
    { Center: Point2D<'Units, 'Coordinates>
      Radius: Quantity<'Units> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Circle2D<'Units, 'Coordinates> as other ->
            this.Center = other.Center
            && this.Radius = other.Radius
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Center, this.Radius)

[<CustomEquality>]
[<NoComparison>]
type Ellipse2D<'Units, 'Coordinates> =
    { Axes: Frame2D<'Units, 'Coordinates, unit>
      XRadius: Quantity<'Units>
      YRadius: Quantity<'Units> }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? (Ellipse2D<'Units, 'Coordinates>) as other ->
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
type Arc2D<'Units, 'Coordinates> =
    { StartPoint: Point2D<'Units, 'Coordinates>
      XDirection: Direction2D<'Coordinates>
      SignedLength: Quantity<'Units>
      SweptAngle: Angle }

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Arc2D<'Units, 'Coordinates> as other ->
            this.StartPoint = other.StartPoint
            && this.XDirection = other.XDirection
            && this.SignedLength = other.SignedLength
            && this.SweptAngle = other.SweptAngle
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.StartPoint, this.XDirection, this.SignedLength, this.SweptAngle)

type Nondegenerate<'Units, 'Coordinates> = Arc2D<'Units, 'Coordinates>

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
type Polygon2D<'Units, 'Coordinates> =
    { OuterLoop: Point2D<'Units, 'Coordinates> list
      InnerLoops: Point2D<'Units, 'Coordinates> list list }

    // Comparable interfaces

    interface IComparable<Polygon2D<'Units, 'Coordinates>> with
        member this.CompareTo(polygon) = this.Comparison(polygon)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Polygon2D<'Units, 'Coordinates> as polygon -> this.Comparison(polygon)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Polygon2D<'Units, 'Coordinates>) =
        this.OuterLoop < other.OuterLoop
        && this.InnerLoops < other.InnerLoops

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Polygon2D<'Units, 'Coordinates> as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Polygon2D<'Units, 'Coordinates>) : bool =
        this.OuterLoop = other.OuterLoop
        && this.InnerLoops = this.InnerLoops

    override this.GetHashCode() =
        HashCode.Combine(hash this.OuterLoop, hash this.InnerLoops)


type Polyline2D<'Units, 'Coordinates> = Polyline2D of Point2D<'Units, 'Coordinates> list
