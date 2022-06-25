(**

---
title: 2D Bounding Box
category: 2D Modules
categoryindex: 3
index: 3
---

# Bounding Box 2D

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian
(***)

(**
# Builders
*)

BoundingBox2D.singleton (Point2D.meters 3. 5.)
(*** include-it ***)

BoundingBox2D.from
    (Point2D.meters 1. -3.)
    (Point2D.meters -5. 2.)
(*** include-it ***)

BoundingBox2D.fromExtrema
    { MinX = Length.meters 2.
      MaxX = Length.meters 4.
      MinY = Length.meters -1.
      MaxY = Length.meters 1. }
(*** include-it ***)

BoundingBox2D.withDimensions
    (Length.meters 7., Length.meters 2.)
    (Point2D.meters -3. 5.)
(*** include-it ***)


(**
# Accessors
*)

let boundingBox : BoundingBox2D<Meters, Cartesian> =
    BoundingBox2D.fromExtrema
        { MinX = Length.meters 2.
          MaxX = Length.meters 4.
          MinY = Length.meters -1.
          MaxY = Length.meters 1. }
(***)

boundingBox.MinX = BoundingBox2D.minX boundingBox
boundingBox.MaxX = BoundingBox2D.maxX boundingBox
boundingBox.MinY = BoundingBox2D.minY boundingBox
boundingBox.MaxY = BoundingBox2D.maxY boundingBox
(***)
    
BoundingBox2D.corners boundingBox
(*** include-it ***)

BoundingBox2D.width boundingBox
(*** include-it ***)

BoundingBox2D.height boundingBox
(*** include-it ***)

BoundingBox2D.dimensions boundingBox
(*** include-it ***)

BoundingBox2D.midX boundingBox
(*** include-it ***)

BoundingBox2D.midY boundingBox
(*** include-it ***)

BoundingBox2D.centerPoint boundingBox
(*** include-it ***)


(**
# Modifiers
*)

BoundingBox2D.containingPoint (Point2D.meters 10. 10.) boundingBox
(*** include-it ***)

BoundingBox2D.containingPoints
    [ Point2D.meters 10. 10.
      Point2D.meters -10. -10. ]
    boundingBox
(*** include-it ***)


BoundingBox2D.scaleAbout
BoundingBox2D.translateBy
BoundingBox2D.translateIn
BoundingBox2D.unsafeOffsetBy
BoundingBox2D.offsetBy
BoundingBox2D.expandBy

(**
# Queries
*)

BoundingBox2D.lineSegments
BoundingBox2D.hull
BoundingBox2D.hullOf
BoundingBox2D.hull3
BoundingBox2D.hullN
BoundingBox2D.hullOfN
BoundingBox2D.aggregate
BoundingBox2D.aggregateOf
BoundingBox2D.aggregate3

BoundingBox2D.isContainedIn
BoundingBox2D.contains
BoundingBox2D.intersects
BoundingBox2D.intersection
BoundingBox2D.overlappingByAtLeast
BoundingBox2D.separatedByAtLeast
BoundingBox2D.union
