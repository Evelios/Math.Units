(**

---
title: 2D Polygon
category: 2D Modules
categoryindex: 3
index: 10
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders
*)

let outerPoints : Point2D<Meters, Cartesian> list =
    [ Point2D.meters 5. 0.
      Point2D.meters 7. 2.
      Point2D.meters 3. 6.
      Point2D.meters 2. 4.
      Point2D.meters 0. 2. ]

Polygon2D.singleLoop outerPoints
(***)

let innerPoints : Point2D<Meters, Cartesian> list =
    [ Point2D.meters 3. 1.
      Point2D.meters 2. 2.
      Point2D.meters 1. 1. ]

Polygon2D.withHoles [ innerPoints ] outerPoints
(***)

Polygon2D.convexHull
(***)

(**
# Accessors
*)

let polygon: Polygon2D<Meters, Cartesian> =
    Polygon2D.withHoles [ innerPoints ] outerPoints

Polygon2D.outerLoop polygon  // or
polygon.OuterLoop
(*** include-it ***)

Polygon2D.innerLoops polygon  // or
polygon.InnerLoops
(*** include-it ***)

Polygon2D.vertices polygon
(***)

Polygon2D.loopEdges
    (Polygon2D.vertices polygon)
(***)

Polygon2D.edges polygon
(***)

Polygon2D.perimeter polygon
(*** include-it ***)

Polygon2D.area polygon
(*** include-it ***)

Polygon2D.centroid polygon
(*** include-it ***)

Polygon2D.boundingBox polygon
(*** include-it ***)

(**
# Modifiers
*)

let invert = false
Polygon2D.mapVertices
    (Point2D.translate (Vector2D.meters 1. 2.))
    invert
    polygon
(***)


Polygon2D.scaleAbout
(***)

Polygon2D.rotateAround
(***)

Polygon2D.translateBy
(***)

Polygon2D.translateIn
(***)

Polygon2D.mirrorAcross
(***)

Polygon2D.translate
(***)

Polygon2D.relativeTo
(***)

Polygon2D.boundingBox
(***)

(**
# Queries
*)

Polygon2D.contains
    (Point2D.meters 2. 2.)
    polygon
(*** include-it ***)

Polygon2D.contains
    (Point2D.meters 10. 10.)
    polygon
(*** include-it ***)
