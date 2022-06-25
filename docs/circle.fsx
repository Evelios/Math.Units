(**

---
title: 2D Circle
category: 2D Modules
categoryindex: 3
index: 4
---

# Circle 2D

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders
*)

Circle2D.atOrigin (Length.meters 2.)
(*** include-it ***)

Circle2D.atPoint
    (Point2D.meters 5. 2.)
    (Length.meters 4.)
(*** include-it ***)

Circle2D.withRadius
    (Length.meters 4.)
    (Point2D.meters 5. 2.) 
(*** include-it ***)

Circle2D.throughPoints
    (Point2D.meters 3. 4.)
    (Point2D.meters -1. 2.)
    (Point2D.meters 4. 1.)
(*** include-it ***)

Circle2D.sweptAround
    Point2D.origin
    (Point2D.meters 3. 3.)
(*** include-it ***)


(**
#Accessors
*)

let circle: Circle2D<Meters, Cartesian> =
    Circle2D.atPoint
        (Point2D.meters 5. 2.)
        (Length.meters 4.)

(***)

Circle2D.centerPoint circle // or
circle.Center
(*** include-it ***)

Circle2D.radius circle // or
circle.Radius
(*** include-it ***)

Circle2D.diameter circle
(*** include-it ***)

Circle2D.area circle
(*** include-it ***)

Circle2D.circumference circle
(*** include-it ***)

Circle2D.toArc circle
(*** include-it ***)

Circle2D.boundingBox circle
(*** include-it ***)

(**
# Modifiers
*)

Circle2D.scaleAbout
Circle2D.translateBy
Circle2D.translateIn
Circle2D.relativeTo
Circle2D.placeIn
Circle2D.containsPoint
Circle2D.intersectsBoundingBox