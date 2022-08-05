[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.LineSegment2D

open Geometry

open Units

// ---- Builders ----

/// Generate a segment segment from two points. This doesn't perform any checks ensuring that the points are not equal.
/// If that is the behavior that you want you should use <see cref="safeFrom"/> function.
let from
    (start: Point2D<'Units, 'Coordinates>)
    (finish: Point2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    { Start = start; Finish = finish }

/// Construct a segment segment from it's two endpoints as a tuple.
let fromEndpoints
    ((start, finish): Point2D<'Units, 'Coordinates> * Point2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    from start finish

/// Safely create a segment segment. This function returns `None` when the two points are almost equal.
/// This has to do with the <see cref="Geometry.Internal.Tolerance"/>.
let safeFrom (start: Point2D<'Units, 'Coordinates>) (finish: Point2D<'Units, 'Coordinates>) =
    if start = finish then
        None
    else
        Some(from start finish)

/// Create a segment segment starting at point in a particular direction and length
let fromPointAndVector (start: Point2D<'Units, 'Coordinates>) (direction: Vector2D<'Units, 'Coordinates>) =
    { Start = start
      Finish = start + direction }

/// Construct a segment segment lying on the given axis, with its endpoints at the
/// given distances from the axis' origin point.
let along
    (axis: Axis2D<'Units, 'Coordinates>)
    (start: Quantity<'Units>)
    (finish: Quantity<'Units>)
    : LineSegment2D<'Units, 'Coordinates> =
    from (Point2D.along axis start) (Point2D.along axis finish)

// ---- Attributes ----

let start (segment: LineSegment2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> = segment.Start

let finish (segment: LineSegment2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> = segment.Finish

let endpoints
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> * Point2D<'Units, 'Coordinates> =
    (segment.Start, segment.Finish)

/// Get the vector from the start point to the end point of the segment segment
let vector (segment: LineSegment2D<'Units, 'Coordinates>) : Vector2D<'Units, 'Coordinates> =
    Vector2D.from segment.Start segment.Finish

let direction (segment: LineSegment2D<'Units, 'Coordinates>) : Direction2D<'Coordinates> option =
    Vector2D.direction (vector segment)

let length (segment: LineSegment2D<'Units, 'Coordinates>) : Quantity<'Units> =
    Point2D.distanceTo segment.Start segment.Finish

let axis (segment: LineSegment2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> option =
    Axis2D.throughPoints segment.Start segment.Finish

/// Get the direction perpendicular to a segment segment, pointing to the left. If
/// the segment segment has zero length, returns `Nothing`.
let perpendicularDirection (segment: LineSegment2D<'Units, 'Coordinates>) : Direction2D<'Coordinates> option =
    Vector2D.direction (Vector2D.perpendicularTo (vector segment))

let midpoint (segment: LineSegment2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
    Point2D.midpoint segment.Start segment.Finish


// ---- Modifiers ----

/// Transform the start and end points of a line segment by a given function
/// and create a new line segment from the resulting points. Most other
/// transformation functions can be defined in terms of `mapEndpoints`
let mapEndpoints
    (f: Point2D<'UnitA, 'CoordinatesA> -> Point2D<'UnitB, 'CoordinatesB>)
    (segment: LineSegment2D<'UnitA, 'CoordinatesA>)
    : LineSegment2D<'UnitB, 'CoordinatesB> =
    from (f segment.Start) (f segment.Finish)

let reverse (segment: LineSegment2D<'Units, 'Coordinates>) = from segment.Finish segment.Start


/// Scale a line segment about the given center point by the given scale.
let scaleAbout
    (point: Point2D<'Units, 'Coordinates>)
    (scale: float)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    mapEndpoints (Point2D.scaleAbout point scale) segment

/// Rotate a line segment counterclockwise around a given center point by a
/// given angle.
let rotateAround
    (centerPoint: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    mapEndpoints (Point2D.rotateAround centerPoint angle) segment


/// Translate a line segment by a given displacement.
let translateBy
    (displacementVector: Vector2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    mapEndpoints (Point2D.translateBy displacementVector) segment


/// Translate a line segment in a given direction by a given distance.
let translateIn
    (translationDirection: Direction2D<'Coordinates>)
    (distance: Quantity<'Units>)
    (lineSegment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    translateBy (Vector2D.withLength distance translationDirection) lineSegment


/// Mirror a line segment across an axis. Note that the endpoints of a mirrored
/// segment are equal to the mirrored endpoints of the original segment, but as a
/// result the normal direction of a mirrored segment is the _opposite_ of the
/// mirrored normal direction of the original segment (since the normal direction is
/// always considered to be 'to the left' of the line segment).
let mirrorAcross
    (axis: Axis2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    mapEndpoints (Point2D.mirrorAcross axis) segment


/// Project a line segment onto an axis.
let projectOnto
    (axis: Axis2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : LineSegment2D<'Units, 'Coordinates> =
    mapEndpoints (Point2D.projectOnto axis) segment


let round (segment: LineSegment2D<'Units, 'Coordinates>) =
    from (Point2D.round segment.Start) (Point2D.round segment.Finish)


// ---- Queries ----

let interpolate (segment: LineSegment2D<'Units, 'Coordinates>) (t: float) : Point2D<'Units, 'Coordinates> =
    Point2D.interpolateFrom segment.Start segment.Finish t

let areParallel (first: LineSegment2D<'Units, 'Coordinates>) (second: LineSegment2D<'Units, 'Coordinates>) : bool =
    match direction first, direction second with
    | Some d1, Some d2 -> d1 = d2 || Direction2D.reverse d1 = d2
    | _ -> false

let isPointOnSegment (point: Point2D<'Units, 'Coordinates>) (segment: LineSegment2D<'Units, 'Coordinates>) =
    let firstDistance = Point2D.distanceTo segment.Start point
    let secondDistance = Point2D.distanceTo segment.Finish point
    (length segment) = (firstDistance + secondDistance)

let distanceToPoint
    (point: Point2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Quantity<'Units> =
    match axis segment with
    | Some segmentAxis ->
        let perpendicular = Point2D.projectOnto segmentAxis point

        // Projected point is on segment
        if isPointOnSegment perpendicular segment then
            Point2D.distanceTo point perpendicular

        // Get the smallest distance between the endpoints
        else
            Length.min (Point2D.distanceTo point segment.Start) (Point2D.distanceTo point segment.Finish)

    // The Line Segment is in the degenerative case where the start and endpoint
    // are the same. So the distance is just point distance.
    | None -> Point2D.distanceTo segment.Start point

/// Get the point on a line segment that is closest to the input point.
let pointClosestTo
    (point: Point2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> =
    if point = segment.Start || point = segment.Finish then
        point

    else

    // Perpendicular projection is the closest point
    if isPointOnSegment point segment then
        point

    else
        match axis segment with
        | Some segmentAxis ->
            let perpendicular = Point2D.projectOnto segmentAxis point

            // Perpendicular projection is the closest point
            if isPointOnSegment perpendicular segment then
                perpendicular
            else if Point2D.distanceSquaredTo point segment.Start < Point2D.distanceSquaredTo point segment.Finish then
                segment.Start
            else
                segment.Finish

        // Segment endpoints are the same
        | None -> point


/// Try to find the intersection between two lines. If the lines are parallel (even if they are overlapping) then no
/// intersection is returned
let intersectionPoint
    (lineSegment1: LineSegment2D<'Units, 'Coordinates>)
    (lineSegment2: LineSegment2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> option =
    // The two line segments are:
    // p |--- r ---| p_
    // q |--- s ---| q_

    let p, p_ = endpoints lineSegment1
    let q, q_ = endpoints lineSegment2
    let r = vector lineSegment1
    let s = vector lineSegment2
    let pq = Vector2D.from p q
    let pq_ = Vector2D.from p q_
    let qp_ = Vector2D.from q p_
    let pqXr = pq |> Vector2D.cross r
    let pqXs = pq |> Vector2D.cross s
    let sXqp_ = s |> Vector2D.cross qp_
    let rXpq_ = r |> Vector2D.cross pq_
    let tDenominator = pqXs - sXqp_
    let uDenominator = pqXr + rXpq_

    if tDenominator = Quantity.zero
       || uDenominator = Quantity.zero then
        // Segments are parallel or collinear.
        // In collinear case, we check if there is only one intersection point.
        if Vector2D.dot s r < Quantity.zero then
            if p_ = q_ then
                // p |----- p_ | q_ -----| q
                Some p_

            else if p = q then
                // q_ |----- q | p -----| p_
                Some p

            else
                None

        else

        if p_ = q then
            // p |----- p_ | q -----| q_
            Some p_

        else if p = q_ then
            // q |----- q_ | p -----| p_
            Some p

        else
            None
    else
        // Segments are not parallel.
        // We search for the intersection point of the two lines.
        let t = pqXs / tDenominator
        let u = pqXr / uDenominator

        if (0. <= t && t <= 1.) && (0. <= u && u <= 1.) then
            // Intersection is within both segments.
            // Ensure interpolation happens from the closest
            // endpoint (this should be more numerically stable, and
            // also mostly ensures that intersection is symmetric)
            let intersection =
                if min t (1. - t) <= min u (1. - u) then
                    interpolate lineSegment1 t

                else
                    interpolate lineSegment2 u

            Some intersection

        else
            None

/// Attempt to find the unique intersection point of a line segment with an
/// axis. If there is no such point (the line segment does not touch the axis, or
/// lies perfectly along it), returns `Nothing`.
let intersectionWithAxis
    (axis: Axis2D<'Units, 'Coordinates>)
    (lineSegment: LineSegment2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> option =

    let p1, p2 = endpoints lineSegment
    let d1 = Point2D.signedDistanceFrom axis p1
    let d2 = Point2D.signedDistanceFrom axis p2
    let product = d1 * d2

    if product < Quantity.zero then
        // The two points are on opposite sides of the axis, so there is a
        // unique intersection point in between them
        let t = (d1 - d2) / d1
        Point2D.interpolateFrom p1 p2 t |> Some

    else

    if product > Quantity.zero then
        // Both points are on the same side of the axis, so no intersection
        // point exists
        None

    else

    if d1 <> Quantity.zero then
        // d2 must be zero since the product is zero, so only p2 is on the axis
        Some p2

    else

    if d2 <> Quantity.zero then
        // d1 must be zero since the product is zero, so only p1 is on the axis
        Some p1

    else if p1 = p2 then
        // Both d1 and d2 are zero, so both p1 and p2 are on the axis but also
        // happen to be equal to each other, so the line segment is actually
        // just a single point on the axis
        Some p1

    else
        // Both endpoints lie on the axis and are not equal to each other - no
        // unique intersection point
        None

///  Measure the distance of a line segment along an axis. This is the range of distances
/// along the axis resulting from projecting the line segment perpendicularly onto the axis.
/// Note that reversing the line segment will _not_ affect the result.
let signedDistanceAlong
    (axis: Axis2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Interval<'Units> =
    Interval.from (Point2D.signedDistanceAlong axis segment.Start) (Point2D.signedDistanceAlong axis segment.Finish)


/// Measure the distance of a line segment from an axis. If the returned interval:
///  - is entirely positive, then the line segment is to the left of the axis
///  - is entirely negative, then the line segment is to the right of the axis
///  - contains zero, then the line segment crosses the axis
/// Note that reversing the line segment will _not_ affect the result.
let signedDistanceFrom
    (axis: Axis2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Interval<'Units> =
    Interval.from (Point2D.signedDistanceFrom axis segment.Start) (Point2D.signedDistanceFrom axis segment.Finish)

/// Take a line segment defined in global coordinates, and return it expressed
/// in local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (segment: LineSegment2D<'Units, 'GlobalCoordinates>)
    : LineSegment2D<'Units, 'LocalCoordinates> =
    mapEndpoints (Point2D.relativeTo frame) segment

/// Take a line segment considered to be defined in local coordinates relative
/// to a given reference frame, and return that line segment expressed in global
/// coordinates.
let placeIn
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (segment: LineSegment2D<'Units, 'GlobalCoordinates>)
    : LineSegment2D<'Units, 'LocalCoordinates> =
    mapEndpoints (Point2D.placeIn frame) segment

/// Get the minimal bounding box containing a given line segment.
let boundingBox (segment: LineSegment2D<'Units, 'Coordinates>) : BoundingBox2D<'Units, 'Coordinates> =
    Internal.BoundingBox2D.from segment.Start segment.Finish
