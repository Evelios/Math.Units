[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Triangle2D

open FSharp.Extensions
open Geometry

// ---- Builders ----

/// Construct a triangle from the first point, to the second, to the third.
let from
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    : Triangle2D<'Unit, 'Coordinates> =

    { P1 = p1; P2 = p2; P3 = p3 }

/// Construct a triangle from its three vertices.
let fromVertices
    (givenVertices: Point2D<'Unit, 'Coordinates> * Point2D<'Unit, 'Coordinates> * Point2D<'Unit, 'Coordinates>)
    : Triangle2D<'Unit, 'Coordinates> =

    Tuple3.map from givenVertices


// ---- Accessors ----

/// Get the three vertices of the triangle
let vertices
    (triangle: Triangle2D<'Unit, 'Coordinates>)
    : Point2D<'Unit, 'Coordinates> * Point2D<'Unit, 'Coordinates> * Point2D<'Unit, 'Coordinates> =

    triangle.P1, triangle.P2, triangle.P3

/// Get the edges of a triangle: from the first vertex to the second, from the
/// second to the third, and from the third back to the first.
let edges
    (triangle: Triangle2D<'Unit, 'Coordinates>)
    : LineSegment2D<'Unit, 'Coordinates> * LineSegment2D<'Unit, 'Coordinates> * LineSegment2D<'Unit, 'Coordinates> =

    (LineSegment2D.fromEndpoints (triangle.P1, triangle.P2),
     LineSegment2D.fromEndpoints (triangle.P2, triangle.P3),
     LineSegment2D.fromEndpoints (triangle.P3, triangle.P1))

/// Get the centroid (center of mass) of a triangle.
let centroid (triangle: Triangle2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    Point2D.centroid3 triangle.P1 triangle.P2 triangle.P3
    
/// Attempt to find the circumcircle of a triangle, a circle that passes through
/// each of the triangle's vertices;
/// If the triangle is degenerate (its three vertices are collinear), returns
/// `None`.
let circumcircle (triangle: Triangle2D<'Unit, 'Coordinates>): Circle2D<'Unit, 'Coordinates> option=
    Circle2D.throughPoints triangle.P1 triangle.P2 triangle.P3
    

// ---- Modifiers ----

/// Transform each vertex of a triangle by a given function and create a new
/// triangle from the resulting points. Most other transformation functions can be
/// defined in terms of `mapVertices`.
let private mapVertices
    (f: Point2D<'UnitA, 'CoordinatesA> -> Point2D<'UnitB, 'CoordinatesB>)
    (triangle: Triangle2D<'UnitA, 'CoordinatesA>)
    : Triangle2D<'UnitB, 'CoordinatesB> =
        
    { P1 = f triangle.P1
      P2 = f triangle.P2
      P3 = f triangle.P3 }
    
/// Scale a triangle about a given point by a given scale. Note that scaling by
/// a negative value will result in the 'winding direction' of the triangle being
/// flipped - if the triangle's vertices were in counterclockwise order before the
/// negative scaling, they will be in clockwise order afterwards and vice versa.
let scaleAbout (point: Point2D<'Unit, 'Coordinates>) (scale: float) (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.scaleAbout point scale) triangle


/// Rotate a triangle around a given point by a given angle.
let rotateAround (centerPoint: Point2D<'Unit, 'Coordinates>) (angle: Angle) (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.rotateAround centerPoint angle) triangle

/// Translate a triangle by a given displacement.
let translateBy (vector: Vector2D<'Unit, 'Coordinates>)  (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.translateBy vector) triangle

/// Translate a triangle in a given direction by a given distance.
let translateIn (direction: Direction2D<'Coordinates>) (distance: Length<'Unit>) (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    translateBy (Vector2D.withLength distance direction) triangle

/// Mirror a triangle across a given axis. Note that mirroring a triangle will
/// result in its 'winding direction' being flipped - if the triangle's vertices
/// were in counterclockwise order before mirroring, they will be in clockwise order
/// afterwards and vice versa.
let mirrorAcross (axis: Axis2D<'Unit, 'Coordinates>) (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.mirrorAcross axis) triangle

/// Take a triangle defined in global coordinates, and return it expressed
/// in local coordinates relative to a given reference frame.
let relativeTo (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (triangle: Triangle2D<'Unit, 'Coordinates>): Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.relativeTo frame) triangle

/// Take a triangle considered to be defined in local coordinates relative to a
/// given reference frame, and return that triangle expressed in global coordinates.
let placeIn (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) (triangle: Triangle2D<'Unit, 'Coordinates>) : Triangle2D<'Unit, 'Coordinates> =
    mapVertices (Point2D.placeIn frame) triangle


// ---- Queries ----

/// Check whether a given point is inside a given triangle. It does not matter
/// whether the triangle's vertices are in clockwise or counterclockwise order.
let contains (point: Point2D<'Unit, 'Coordinates>) (triangle: Triangle2D<'Unit, 'Coordinates>) : bool =
    let crossProduct startVertex endVertex =
        let vectorToPoint = Vector2D.from startVertex point
        let segmentVector = Vector2D.from startVertex endVertex
        segmentVector |> Vector2D.cross vectorToPoint

    let firstProduct = crossProduct triangle.P1 triangle.P2
    let secondProduct = crossProduct triangle.P2 triangle.P3
    let thirdProduct = crossProduct triangle.P3 triangle.P1

    ((firstProduct >= Length.zero)
     && (secondProduct >= Length.zero)
     && (thirdProduct >= Length.zero))
    || ((firstProduct <= Length.zero)
        && (secondProduct <= Length.zero)
        && (thirdProduct <= Length.zero))

/// Get the signed area of a triangle, returning a positive value if the
/// triangle's vertices are in counterclockwise order and a negative value
/// otherwise.
let counterclockwiseArea (triangle: Triangle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    let firstVector = Vector2D.from triangle.P1 triangle.P2
    let secondVector = Vector2D.from triangle.P1 triangle.P3
    0.5 * (firstVector |> Vector2D.cross secondVector)

/// Get the signed area of a triangle, returning a positive value if the
/// triangle's vertices are in clockwise order and a negative value otherwise.
let clockwiseArea (triangle: Triangle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> = -(counterclockwiseArea triangle)

/// Get the area of a triangle. The result will always be positive regardless of
/// whether the triangle's vertices are in clockwise or counterclockwise order.
let area (triangle: Triangle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    counterclockwiseArea triangle |> Length.abs

