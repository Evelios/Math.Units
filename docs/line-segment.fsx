(**

---
title: 2D Line Segment
category: 2D Modules
categoryindex: 3
index: 8
---

# Line Segment 2D

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders
*)

LineSegment2D.from
    (Point2D.meters 3. 4.)
    (Point2D.meters -8. 2.)
(***)
    
LineSegment2D.fromEndpoints
    (Point2D.meters 3. 4.,
     Point2D.meters -8. 2.)
(***)
    
LineSegment2D.safeFrom
    (Point2D.meters 3. 4.)
    (Point2D.meters -8. 2.)
(*** hide ***)
LineSegment2D.safeFrom<Meters, Cartesian>
    (Point2D.meters 3. 4.)
    (Point2D.meters -8. 2.)
(*** include-it ***)

LineSegment2D.safeFrom
    (Point2D.meters 2. 1.)
    (Point2D.meters 2. 1.)
(*** hide ***)
LineSegment2D.safeFrom<Meters, Cartesian>
    (Point2D.meters 2. 1.)
    (Point2D.meters 2. 1.)
(*** include-it ***)

LineSegment2D.fromPointAndVector
    Point2D.origin
    (Vector2D.meters 7. 8.)
(***)

LineSegment2D.along
    Axis2D.x
    (Length.meters 3.)
    (Length.meters 7.)
(***)

(**
# Accessors
*)

let segment: LineSegment2D<Meters, Cartesian> =
    LineSegment2D.from
        (Point2D.meters 3. 4.)
        (Point2D.meters -8. 2.)
(***)

LineSegment2D.start segment  // or
segment.Start
(*** include-it ***)

LineSegment2D.finish segment  // or
segment.Finish
(*** include-it ***)

LineSegment2D.endpoints segment
(*** include-it ***)

LineSegment2D.vector segment
(*** include-it ***)

LineSegment2D.direction segment
(*** include-it ***)

LineSegment2D.length segment
(*** include-it ***)

LineSegment2D.axis segment
(*** include-it ***)

LineSegment2D.perpendicularDirection segment
(*** include-it ***)

LineSegment2D.midpoint segment
(*** include-it ***)

(**
# Modifiers
*)

LineSegment2D.mapEndpoints
    (Point2D.plus (Vector2D.meters 4. 2.))
(***)

LineSegment2D.reverse segment
(*** include-it ***)

LineSegment2D.scaleAbout
(***)

LineSegment2D.rotateAround
(***)

LineSegment2D.translateBy
(***)

LineSegment2D.translateIn
(***)

LineSegment2D.mirrorAcross
(***)

LineSegment2D.projectOnto
(***)

LineSegment2D.round

(**
# Queries
*)

LineSegment2D.interpolate 
(***)

LineSegment2D.areParallel
(***)

LineSegment2D.isPointOnSegment
(***)

LineSegment2D.distanceToPoint
(***)

LineSegment2D.pointClosestTo
(***)

LineSegment2D.intersectionPoint
(***)

LineSegment2D.intersectionWithAxis
(***)

LineSegment2D.signedDistanceAlong
(***)

LineSegment2D.signedDistanceFrom
(***)

LineSegment2D.relativeTo
(***)

LineSegment2D.placeIn
(***)

LineSegment2D.boundingBox
(***)
