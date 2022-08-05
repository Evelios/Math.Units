[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Polyline2D

open Units

let fromVertices (vertices: Point2D<'Units, 'Coordinates> list) : Polyline2D<'Units, 'Coordinates> = Polyline2D vertices

let vertices (polyline: Polyline2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> list =
    match polyline with
    | Polyline2D vertices -> vertices

let segments (polyline: Polyline2D<'Units, 'Coordinates>) : LineSegment2D<'Units, 'Coordinates> list =
    vertices polyline
    |> List.pairwise
    |> List.map LineSegment2D.fromEndpoints

let length (polyline: Polyline2D<'Units, 'Coordinates>) : Quantity<'Units> =
    segments polyline
    |> List.map LineSegment2D.length
    |> Quantity.sum

// Transform each vertex of a polyline by the given function. All other
// transformations can be defined in terms of `mapVertices`; for example,
//     Polyline2D.mirrorAcross axis
// is equivalent to
//     Polyline2D.mapVertices (Point2d.mirrorAcross axis)
let mapVertices (f: Point2D<'Units, 'Coordinates> -> Point2D<'Units, 'Coordinates>) polyline =
    vertices polyline |> List.map f |> fromVertices

// Scale a polyline about a given center point by a given scale.
let scaleAbout
    (point: Point2D<'Units, 'Coordinates>)
    (scale: float)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.scaleAbout point scale) polyline

// Rotate a polyline around the given center point counterclockwise by the
// given angle.
let rotateAround
    (point: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.rotateAround point angle) polyline

// Translate a polyline by the given displacement.
let translateBy
    (vector: Vector2D<'Units, 'Coordinates>)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.translateBy vector) polyline

// Translate a polyline in a given direction by a distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Quantity<'Units>)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.translateIn direction distance) polyline

// Mirror a polyline across the given axis.
let mirrorAcross
    (axis: Axis2D<'Units, 'Coordinates>)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.mirrorAcross axis) polyline

// Project (flatten) a polyline onto the given axis.
let projectOnto
    (axis: Axis2D<'Units, 'Coordinates>)
    (polyline: Polyline2D<'Units, 'Coordinates>)
    : Polyline2D<'Units, 'Coordinates> =
    mapVertices (Point2D.projectOnto axis) polyline

// Take a polyline defined in global coordinates, and return it expressed
// in local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (polyline: Polyline2D<'Units, 'GlobalCoordinates>)
    : Polyline2D<'Units, 'LocalCoordinates> =
    mapVertices (Point2D.relativeTo frame) polyline

// Take a polyline considered to be defined in local coordinates relative
// to a given reference frame, and return that polyline expressed in global
// coordinates.
let placeIn
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (polyline: Polyline2D<'Units, 'GlobalCoordinates>)
    : Polyline2D<'Units, 'LocalCoordinates> =
    mapVertices (Point2D.placeIn frame) polyline

// Get the minimal bounding box containing a given polyline. Returns `Nothing`
// if the polyline has no vertices.
let boundingBox (polyline: Polyline2D<'Units, 'Coordinates>) : BoundingBox2D<'Units, 'Coordinates> option =
    BoundingBox2D.hullN (vertices polyline)

let private refineBySegment
    (polylineLength: Quantity<'Units>)
    (roughCentroid: Point2D<'Units, 'Coordinates>)
    (currentCentroid: Point2D<'Units, 'Coordinates>)
    (segment: LineSegment2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> =
    let segmentMidpoint = LineSegment2D.midpoint segment

    let segmentLength = LineSegment2D.length segment

    let offset =
        Vector2D.from roughCentroid segmentMidpoint
        |> Vector2D.scaleBy (segmentLength / polylineLength)

    currentCentroid |> Point2D.translateBy offset

let centroid (polyline: Polyline2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> option =

    match vertices polyline, boundingBox polyline with
    | [], _ -> None
    | _, None -> None

    | first :: _, Some box ->
        let polylineLength = length polyline

        if polylineLength = Quantity.zero then
            Some first

        else
            let roughCentroid = BoundingBox2D.centerPoint box

            let helper =
                refineBySegment polylineLength roughCentroid

            segments polyline
            |> List.fold helper roughCentroid
            |> Some
