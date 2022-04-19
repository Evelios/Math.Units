[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.BoundingBox2D

open Geometry


// ---- Builders ----

/// Creates an infinitely small bounding box. This can be used when growing a bounding box around objects
let empty<'Unit, 'Coordinates> : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = Length.create<'Unit> infinity
      MaxX = Length.create<'Unit> -infinity
      MinY = Length.create<'Unit> infinity
      MaxY = Length.create<'Unit> -infinity }

/// Create a bounding box that contains the two points
let from (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) =
    { MinX = min p1.X p2.X
      MaxX = max p1.X p2.X
      MinY = min p1.Y p2.Y
      MaxY = max p1.Y p2.Y }

/// If the minimum and maximum values are provided in the wrong order (for example
/// if `minX` is greater than `maxX`), then they will be swapped so that the
/// resulting bounding box is valid.
let fromExtrema given : BoundingBox2D<'Unit, 'Coordinates> =
    if (given.MinX <= given.MaxX)
       && (given.MinY <= given.MaxY) then
        given

    else
        { MinX = Length.min given.MinX given.MaxX
          MaxX = Length.max given.MinX given.MaxX
          MinY = Length.min given.MinY given.MaxY
          MaxY = Length.max given.MinY given.MaxY }

// /Construct a bounding box given its overall dimensions (width and height)
// /and center point.
let withDimensions
    (givenWidth: Length<'Unit>, givenHeight: Length<'Unit>)
    (givenCenterPoint: Point2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let x0, y0 = Point2D.coordinates givenCenterPoint
    let halfWidth = Length.half (Length.abs givenWidth)
    let halfHeight = Length.half (Length.abs givenHeight)

    { MinX = x0 - halfWidth
      MaxX = x0 + halfWidth
      MinY = y0 - halfHeight
      MaxY = y0 + halfHeight }

let singleton (p: Point2D<'Unit, 'Coordinastes>) : BoundingBox2D<'Unit, 'Coordinates> = from p p


// ---- Accessors ----

/// Returned in clockwise order from top left rotating around clockwise
let corners (box: BoundingBox2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> list =
    [ box.TopLeft
      box.TopRight
      box.BottomRight
      box.BottomLeft ]

let width (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MaxX - box.MinX

let height (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MaxY - box.MinY

let minX (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MinX

let maxX (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MaxX

let minY (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MinY

let maxY (box: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = box.MaxY

/// Get the X and Y dimensions (width and height) of a bounding box.
let dimensions (boundingBox: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> * Length<'Unit> =
    (boundingBox.MaxX - boundingBox.MinX, boundingBox.MaxY - boundingBox.MinY)

let midX (box: BoundingBox2D<'Unit, 'Coordiantes>) : Length<'Unit> = box.MaxX - box.MinX / 2.

let midY (box: BoundingBox2D<'Unit, 'Coordiantes>) : Length<'Unit> = box.MaxY - box.MinY / 2.

let centerPoint (box: BoundingBox2D<'Unit, 'Coordiantes>) : Point2D<'Unit, 'Coodinates> =
    Point2D.xy (midX box) (midY box)


// ---- Modifiers ----

/// Get a bounding box that contains the new point. If the box does not contain the new point, the box will grow
/// to fit the new point. If the point is within the box, the same bounding box is returned.
let containingPoint
    (point: Point2D<'Unit, 'Coordinates>)
    (box: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = min box.MinX point.X
      MaxX = max box.MaxX point.X
      MinY = min box.MinY point.Y
      MaxY = max box.MaxY point.Y }

let containingPoints
    (points: Point2D<'Unit, 'Coordinates> list)
    (box: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    Seq.fold (fun box point -> containingPoint point box) box points

/// Scale a bounding box about a given point by a given scale.
let scaleAbout
    (point: Point2D<'Unit, 'Coordinates>)
    (scale: float)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let scaleAbout x0 scale x = x0 + scale * (x - x0)

    let x0 = point.X
    let y0 = point.Y
    let scaledMinX = scaleAbout x0 scale (minX boundingBox)
    let scaledMaxX = scaleAbout x0 scale (maxX boundingBox)
    let scaledMinY = scaleAbout y0 scale (minY boundingBox)
    let scaledMaxY = scaleAbout y0 scale (maxY boundingBox)

    if scale >= 0. then
        { MinX = scaledMinX
          MaxX = scaledMaxX
          MinY = scaledMinY
          MaxY = scaledMaxY }

    else
        { MinX = scaledMaxX
          MaxX = scaledMinX
          MinY = scaledMaxY
          MaxY = scaledMinY }

/// Translate a bounding box by a given displacement.
let translateBy
    (displacement: Vector2D<'Unit, 'Coordinates>)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let dx = displacement.X
    let dy = displacement.Y

    { MinX = boundingBox.MinX + dx
      MaxX = boundingBox.MaxX + dx
      MinY = boundingBox.MinY + dy
      MaxY = boundingBox.MaxY + dy }


/// Translate a bounding box in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Length<'Unit>)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    translateBy (Vector2D.withLength distance direction) boundingBox

/// Offsets boundingBox irrespective of the resulting bounding box is valid or
/// not.
let unsafeOffsetBy
    (amount: Length<'Unit>)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    { MinX = boundingBox.MinX - amount
      MinY = boundingBox.MinY - amount
      MaxY = boundingBox.MaxY + amount
      MaxX = boundingBox.MaxX + amount }

/// Expand or shrink the given bounding box in all the directions by the given
/// distance. A positive offset will cause the bounding box to expand and a negative
/// value will cause it to shrink.
/// Returns `None` if the offset is negative and large enough to cause the
/// bounding box to vanish (that is, if the offset is larger than half the height or
/// half the width of the bounding box, whichever is less).
/// If you only want to expand a bounding box, you can use
/// [`expandBy`](BoundingBox2d#expandBy) instead (which does not return an `Option`).
let offsetBy
    (amount: Length<'Unit>)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> option =

    let width, height = dimensions boundingBox
    let minValidOffset = (Length.min width height) * -0.5

    if amount > minValidOffset then
        Some <| unsafeOffsetBy amount boundingBox
    else
        None

/// Expand the given bounding box in all directions by the given offset:
/// Negative offsets will be treated as positive (the absolute value will be used),
/// so the resulting box will always be at least as large as the original. If you
/// need to be able to contract a bounding box, use
/// [`offsetBy`](BoundingBox2d#offsetBy) instead.
let expandBy
    (amount: Length<'Unit>)
    (boundingBox: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    unsafeOffsetBy (Length.abs amount) boundingBox



// ---- Queries ----

/// Get the four line segments surrounding the bounding box. The lines are created from the top left point, creating
/// line segments around the bounding box clockwise.
let lineSegments (box: BoundingBox2D<'Unit, 'Coordinates>) =
    [ LineSegment2D.from box.TopLeft box.TopRight
      LineSegment2D.from box.TopRight box.BottomRight
      LineSegment2D.from box.BottomRight box.BottomLeft
      LineSegment2D.from box.BottomLeft box.TopLeft ]

/// Find the bounding box containing one or more input points.
/// Often ends up being used within a match expression.
/// See also [`hullN`](#hullN).
let hull
    (first: Point2D<'Unit, 'Coordinates>)
    (rest: Point2D<'Unit, 'Coordinates> list)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let rec hullHelp
        (currentMinX: Length<'Unit>)
        (currentMaxX: Length<'Unit>)
        (currentMinY: Length<'Unit>)
        (currentMaxY: Length<'Unit>)
        (points: Point2D<'Unit, 'Coordinates> list)
        : BoundingBox2D<'Unit, 'Coordinates> =

        match points with
        | next :: rest ->
            hullHelp
                (min next.X currentMinX)
                (max next.X currentMaxX)
                (min next.Y currentMinY)
                (max next.Y currentMaxY)
                rest

        | [] ->
            { MinX = currentMinX
              MaxX = currentMaxX
              MinY = currentMinY
              MaxY = currentMaxY }

    let x, y = Point2D.coordinates first

    hullHelp x x y y rest

/// Like [`hull`](#hull), but lets you work on any kind of item as long as a
/// point can be extracted from it. For example, if you had
let hullOf
    (getPoint: 'a -> Point2D<'Unit, 'Coordinates>)
    (first: 'a)
    (rest: 'a list)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let rec hullOfHelp
        (currentMinX: Length<'Unit>)
        (currentMaxX: Length<'Unit>)
        (currentMinY: Length<'Unit>)
        (currentMaxY: Length<'Unit>)
        (list: 'a list)
        : BoundingBox2D<'Unit, 'Coordinates> =

        match list with
        | next :: rest ->
            let x, y = getPoint next |> Point2D.coordinates

            hullOfHelp (min x currentMinX) (max x currentMaxX) (min y currentMinY) (max y currentMaxY) rest

        | [] ->
            { MinX = currentMinX
              MaxX = currentMaxX
              MinY = currentMinY
              MaxY = currentMaxY }

    let x, y = getPoint first |> Point2D.coordinates

    hullOfHelp x x y y rest


/// Build a bounding box that contains all three of the given points.
/// This is equivalent to calling `hull` on three points but is more efficient.
let hull3
    (firstPoint: Point2D<'Unit, 'Coordiantes>)
    (secondPoint: Point2D<'Unit, 'Coordinates>)
    (thirdPoint: Point2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let x1 = firstPoint.X
    let y1 = firstPoint.Y
    let x2 = secondPoint.X
    let y2 = secondPoint.Y
    let x3 = thirdPoint.X
    let y3 = thirdPoint.Y

    { MinX = Length.min x1 (Length.min x2 x3)
      MaxX = Length.max x1 (Length.max x2 x3)
      MinY = Length.min y1 (Length.min y2 y3)
      MaxY = Length.max y1 (Length.max y2 y3) }


/// Construct a bounding box containing all _N_ points in the given list. If the
/// list is empty, returns `Nothing`. If you know you have at least one point, you
/// can use [`hull`](#hull) instead.
let hullN (points: Point2D<'Unit, 'Coordinates> list) : BoundingBox2D<'Unit, 'Coordinates> option =
    match points with
    | first :: rest -> Some(hull first rest)
    | [] -> None

/// Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
let hullOfN
    (getBoundingBox: 'a -> Point2D<'Unit, 'Coordinates>)
    (items: 'a list)
    : BoundingBox2D<'Unit, 'Coordinates> option =
    match items with
    | first :: rest -> Some(hullOf getBoundingBox first rest)
    | [] -> None

/// Find the bounding box containing one or more input boxes; works much like
/// [`hull`](#hull). See also [`aggregateN`](#aggregateN).
let aggregate
    (first: BoundingBox2D<'Unit, 'Coordinates>)
    (rest: BoundingBox2D<'Unit, 'Coordinates> list)
    : BoundingBox2D<'Unit, 'Coordinates> =

    let rec aggregateHelp currentMinX currentMaxX currentMinY currentMaxY boxes =
        match boxes with
        | next :: rest ->
            aggregateHelp
                (Length.min next.MinX currentMinX)
                (Length.max next.MaxX currentMaxX)
                (Length.min next.MinY currentMinY)
                (Length.max next.MaxY currentMaxY)
                rest

        | [] ->
            { MinX = currentMinX
              MaxX = currentMaxX
              MinY = currentMinY
              MaxY = currentMaxY }

    aggregateHelp first.MinX first.MaxX first.MinY first.MaxY rest

/// Like [`aggregate`](#aggregate), but lets you work on any kind of item as
/// long as a bounding box can be extracted from it.
let aggregateOf
    (getBoundingBox: 'a -> BoundingBox2D<'Unit, 'Coordinates>)
    (first: 'a)
    (rest: 'a list)
    : BoundingBox2D<'Unit, 'Coordinates> =
    let rec aggregateOfHelp currentMinX currentMaxX currentMinY currentMaxY getBoundingBox items =

        match items with
        | next :: rest ->
            let b = getBoundingBox next

            aggregateOfHelp
                (Length.min b.MinX currentMinX)
                (Length.max b.MaxX currentMaxX)
                (Length.min b.MinY currentMinY)
                (Length.max b.MaxY currentMaxY)
                getBoundingBox
                rest

        | [] ->
            { MinX = currentMinX
              MaxX = currentMaxX
              MinY = currentMinY
              MaxY = currentMaxY }


    let b1 = (getBoundingBox first)
    aggregateOfHelp b1.MinX b1.MaxX b1.MinY b1.MaxY getBoundingBox rest

/// Build a bounding box that contains all three of the given bounding boxes.
/// This is equivalent to running `aggregate` with three points but is more
/// efficient.
let aggregate3
    (b1: BoundingBox2D<'Unit, 'Coordinates>)
    (b2: BoundingBox2D<'Unit, 'Coordinates>)
    (b3: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = Length.min b1.MinX (Length.min b2.MinX b3.MinX)
      MaxX = Length.max b1.MaxX (Length.max b2.MaxX b3.MaxX)
      MinY = Length.min b1.MinY (Length.min b2.MinY b3.MinY)
      MaxY = Length.max b1.MaxY (Length.max b2.MaxY b3.MaxY) }


/// Construct a bounding box containing all bounding boxes in the given list. If
/// the list is empty, returns `Nothing`. If you know you have at least one bounding
/// box, you can use [`aggregate`](#aggregate) instead.
let aggregateN (boxes: BoundingBox2D<'Unit, 'Coordinates> list) : BoundingBox2D<'Unit, 'Coordinates> option =
    match boxes with
    | first :: rest -> Some(aggregate first rest)
    | [] -> None


/// Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
let aggregateOfN
    (getBoundingBox: 'a -> BoundingBox2D<'Unit, 'Coordinates>)
    (items: 'a list)
    : BoundingBox2D<'Unit, 'Coordinates> option =

    match items with
    | first :: rest -> Some(aggregateOf getBoundingBox first rest)
    | [] -> None
    
/// Test to see if the target point is contained withing the bounding box
let contains (target: Point2D<'Unit, 'Coordinates>) (box: BoundingBox2D<'Unit, 'Coordinates>) : bool =
    target.X >= box.MinX
    && target.X <= box.MaxX
    && target.Y >= box.MinY
    && target.Y <= box.MaxY

/// Test to see if the target bounding box is contained withing the bounding box
let containsBoundingBox (target: BoundingBox2D<'Unit, 'Coordinates>) (box: BoundingBox2D<'Unit, 'Coordinates>) : bool =
    target.MinX >= box.MinX
    && target.MaxX <= box.MaxX
    && target.MinY >= box.MinY
    && target.MaxY <= box.MaxY

let intersects (first: BoundingBox2D<'Unit, 'Coordinates>) (second: BoundingBox2D<'Unit, 'Coordinates>) : bool =
    first.MinX <= second.MaxX
    && first.MaxX >= second.MinX
    && first.MinY <= second.MaxY
    && first.MaxY >= second.MinY

/// Attempt to build a bounding box that contains all points common to both
/// given bounding boxes. If the given boxes do not intersect, returns `Nothing`.
/// If two boxes just touch along an edge or at a corner, they are still considered
/// to have an intersection, even though that intersection will have zero area (at
/// least one of its dimensions will be zero).
let intersection
    (first: BoundingBox2D<'Unit, 'Coordinates>)
    (second: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> option =

    if intersects first second then
        Some
            { MinX = max first.MinX second.MinX
              MaxX = min first.MaxX second.MaxX
              MinY = max first.MinY second.MinY
              MaxY = min first.MaxY second.MaxY }
    else
        None

/// Check two boxes overlap by at least the given amount. For example, you could
/// implement a tolerant collision check (one that only returns true if the boxes
/// overlap by at least a millimeter, and ignores boxes that just barely touch each
/// other).
///
/// Overlap is defined as the _minimum_ distance one box would have to move so that
/// it did not touch the other. Boxes that just touch are considered to have an
/// overlap of zero.
let overlappingByAtLeast
    (tolerance: Length<'Unit>)
    (firstBox: BoundingBox2D<'Unit, 'Coordinates>)
    (secondBox: BoundingBox2D<'Unit, 'Coordinates>)
    : bool =

    let xOverlap =
        Length.min (maxX firstBox) (maxX secondBox)
        - Length.max (minX firstBox) (minX secondBox)

    let yOverlap =
        Length.min (maxY firstBox) (maxY secondBox)
        - Length.max (minY firstBox) (minY secondBox)

    let clampedTolerance = Length.max tolerance Length.zero

    xOverlap >= clampedTolerance
    && yOverlap >= clampedTolerance

/// Check if two boxes are separated by at least the given amount. For example,
/// to perform clash detection between some objects, you could use `separatedBy` on
/// those objects' bounding boxes as a quick check to see if the objects had a gap
/// of at least 1 cm between them.
/// Separation is defined as the _minimum_ distance one box would have to move so
/// that it touched the other. (Note that this may be a _diagonal_ distance between
/// corners.) Boxes that just touch are considered to have a separation of zero.
/// will return true even if the two boxes just touch each other.
let separatedByAtLeast
    (tolerance: Length<'Unit>)
    (firstBox: BoundingBox2D<'Unit, 'Coordinates>)
    (secondBox: BoundingBox2D<'Unit, 'Coordinates>)
    : bool =

    let clampedTolerance = Length.max tolerance Length.zero

    let xSeparation =
        Length.max (minX firstBox) (minX secondBox)
        - Length.min (maxX firstBox) (maxX secondBox)

    let ySeparation =
        Length.max (minY firstBox) (minY secondBox)
        - Length.min (maxY firstBox) (maxY secondBox)

    if xSeparation > Length.zero
       && ySeparation > Length.zero then
        Length.square xSeparation
        + Length.square ySeparation
        >= Length.square clampedTolerance

    else if xSeparation > Length.zero then
        xSeparation >= clampedTolerance

    else if ySeparation > Length.zero then
        ySeparation >= clampedTolerance

    else if xSeparation = Length.zero
            || ySeparation = Length.zero then
        clampedTolerance = Length.zero

    else
        false

/// Test if the second given bounding box is fully contained within the first
/// (is a subset of it).
let isContainedIn (other: BoundingBox2D<'Unit, 'Coordinates>) (box: BoundingBox2D<'Unit, 'Coordaintes>) : bool =
    (other.MinX <= box.MinX)
    && (box.MaxX <= other.MaxX)
    && (other.MinY <= box.MinY)
    && (box.MaxY <= other.MaxY)



// ---- Boolean Operations ----

/// Create a bounding box that contains both bounding boxes.
let union
    (first: BoundingBox2D<'Unit, 'Coordinates>)
    (second: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = min first.MinX second.MinX
      MaxX = max first.MaxX second.MaxX
      MinY = min first.MinY second.MinY
      MaxY = max first.MaxY second.MaxY }
