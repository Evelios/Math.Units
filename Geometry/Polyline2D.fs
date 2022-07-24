[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Polyline2D

let fromVertices (vertices: Point2D<'Unit, 'Coordinates> list) : Polyline2D<'Unit, 'Coordinates> = Polyline2D vertices

let vertices (polyline: Polyline2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> list =
    match polyline with
    | Polyline2D vertices -> vertices

let segments (polyline: Polyline2D<'Unit, 'Coordinates>) : LineSegment2D<'Unit, 'Coordinates> list =
    vertices polyline
    |> List.pairwise
    |> List.map LineSegment2D.fromEndpoints

let length (polyline: Polyline2D<'Unit, 'Coordinates>) : Quantity<'Unit> =
    segments polyline
    |> List.map LineSegment2D.length
    |> Length.sum

// Transform each vertex of a polyline by the given function. All other
// transformations can be defined in terms of `mapVertices`; for example,
//     Polyline2D.mirrorAcross axis
// is equivalent to
//     Polyline2D.mapVertices (Point2d.mirrorAcross axis)
let mapVertices (f: Point2D<'Unit, 'Coordinates> -> Point2D<'Unit, 'Coordinates>) polyline =
    vertices polyline |> List.map f |> fromVertices

// Scale a polyline about a given center point by a given scale.
let scaleAbout
    (point: Point2D<'Unit, 'Coordinates>)
    (scale: float)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.scaleAbout point scale) polyline

// Rotate a polyline around the given center point counterclockwise by the
// given angle.
let rotateAround
    (point: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.rotateAround point angle) polyline

// Translate a polyline by the given displacement.
let translateBy
    (vector: Vector2D<'Unit, 'Coordinates>)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.translateBy vector) polyline

// Translate a polyline in a given direction by a distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Quantity<'Unit>)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.translateIn direction distance) polyline

// Mirror a polyline across the given axis.
let mirrorAcross
    (axis: Axis2D<'Unit, 'Coordinates>)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.mirrorAcross axis) polyline

// Project (flatten) a polyline onto the given axis.
let projectOnto
    (axis: Axis2D<'Unit, 'Coordinates>)
    (polyline: Polyline2D<'Unit, 'Coordinates>)
    : Polyline2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.projectOnto axis) polyline

// Take a polyline defined in global coordinates, and return it expressed
// in local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'GlobalCoordinates, 'Defines>)
    (polyline: Polyline2D<'Unit, 'GlobalCoordinates>)
    : Polyline2D<'Unit, 'LocalCoordinates> =
    mapVertices (Point2D.relativeTo frame) polyline

// Take a polyline considered to be defined in local coordinates relative
// to a given reference frame, and return that polyline expressed in global
// coordinates.
let placeIn
    (frame: Frame2D<'Unit, 'GlobalCoordinates, 'Defines>)
    (polyline: Polyline2D<'Unit, 'GlobalCoordinates>)
    : Polyline2D<'Unit, 'LocalCoordinates> =
    mapVertices (Point2D.placeIn frame) polyline

// Get the minimal bounding box containing a given polyline. Returns `Nothing`
// if the polyline has no vertices.
let boundingBox (polyline: Polyline2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> option =
    BoundingBox2D.hullN (vertices polyline)

let private refineBySegment
    (polylineLength: Quantity<'Unit>)
    (roughCentroid: Point2D<'Unit, 'Coordinates>)
    (currentCentroid: Point2D<'Unit, 'Coordinates>)
    (segment: LineSegment2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> =
    let segmentMidpoint = LineSegment2D.midpoint segment

    let segmentLength = LineSegment2D.length segment

    let offset =
        Vector2D.from roughCentroid segmentMidpoint
        |> Vector2D.scaleBy (segmentLength / polylineLength)

    currentCentroid |> Point2D.translateBy offset

let centroid (polyline: Polyline2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> option =

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
