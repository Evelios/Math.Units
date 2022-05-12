(**

---
title: 2D Axis
category: 2D Modules
categoryindex: 3
index: 2
---

# Axis 2D

An axis is a line that goes through a point at a particular angle.

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Constants
*)

Axis2D.x
Axis2D.y

(**
# Builders

For the following, the input direction that will be used is
*)
let direction: Direction2D<Cartesian> =
    Direction2D.fromAngle (Angle.degrees 45.)
(*** hide ***)
direction
(*** include-it ***)

Axis2D.through (Point2D.meters 3. 3.) direction
(*** include-it ***)

Axis2D.withDirection direction (Point2D.meters 3. 3.)
(*** include-it ***)

Axis2D.throughPoints
    (Point2D.meters 3. 3.)
    (Point2D.meters 5. 5.)
(*** include-it ***)

(**
# Accessors

To show the output of the accessor methods, the following axis will be used.
*)

let axis =
   Axis2D.through
       (Point2D.meters 3. 3.)
       direction
(*** hide ***)
axis
(*** include-it ***)


Axis2D.originPoint axis  // or
axis.Origin
(*** include-it ***)

Axis2D.direction axis  // or
axis.Direction
(*** include-it ***)


(**
# Modifiers
*)

Axis2D.reverse axis
(*** include-it ***)

Axis2D.moveTo (Point2D.meters -1. 4.) axis
(*** include-it ***)

Axis2D.rotateAround Point2D.origin Angle.pi axis
(*** include-it ***)

Axis2D.rotateBy
Axis2D.translateBy
Axis2D.translateIn
Axis2D.mirrorAcross
Axis2D.relativeTo
Axis2D.placeIn

