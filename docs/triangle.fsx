(**
---
title: 2D Triangle
category: 2D Modules
categoryindex: 3
index: 13
---
*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders

For the builder functions, these points are defined to make the examples shorter.
*)

let p1 : Point2D<Meters, Cartesian> =
    Point2D.meters 1. 3.

let p2 : Point2D<Meters, Cartesian> =
    Point2D.meters 4. 5.
    
let p3 : Point2D<Meters, Cartesian> =
    Point2D.meters 7. 8.

(***)

Triangle2D.from p1 p2 p3
(***)

Triangle2D.fromVertices (p1, p2, p3)
(***)

(**
# Accessors
*)

let triangle =  
    Triangle2D.from p1 p2 p3
    
(***)

Triangle2D.vertices triangle
(*** include-it ***)

Triangle2D.edges triangle
(*** include-it ***)

Triangle2D.centroid triangle
(*** include-it ***)

Triangle2D.circumcircle triangle
(*** include-it ***)

(**
# Modifiers
*)

let referencePoint = Point2D.meters 1. 1.

(***)

Triangle2D.scaleAbout
    referencePoint 2.
    triangle
(*** include-it ***)

Triangle2D.rotateAround
    referencePoint
    Angle.halfPi
    triangle
(*** include-it ***)

Triangle2D.translateBy
    (Vector2D.meters 4. 4.)
    triangle
(*** include-it ***)

Triangle2D.translateIn
    (Direction2D.degrees 30.)
    (Length.meters 2.)
    triangle
(*** include-it ***)

Triangle2D.mirrorAcross Axis2D.x triangle
(*** include-it ***)

Triangle2D.placeIn
(***)

(**
# Queries
*)

Triangle2D.contains (Point2D.meters 2. 2.) triangle
(*** include-it ***)

Triangle2D.contains (Point2D.meters 4. 5.) triangle
(*** include-it ***)

(**
Get the signed area of the triangle. The value is positive is the triangles
vertices are in counterclockwise order.
*)
Triangle2D.counterclockwiseArea triangle
(*** include-it ***)

(**
Get the signed area of the triangle. The value is positive is the triangles
vertices are in clockwise order.
*)
Triangle2D.clockwiseArea triangle
(*** include-it ***)

(**
Get the area of the triangle. This value is always positive.
*)
Triangle2D.area triangle
(*** include-it ***)

