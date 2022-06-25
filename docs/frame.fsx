(**

---
title: 2D Frame
category: 2D Modules
categoryindex: 3
index: 7
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders
*)

Frame2D.atOrigin
(***)

Frame2D.atPoint (Point2D.meters 4. 2.)
(***)

Frame2D.withXDirection
    (Direction2D.fromAngle (Angle.degrees 30.))
    (Point2D.meters 3. 1.)
(***)

Frame2D.withYDirection
    (Direction2D.fromAngle (Angle.degrees -60.))
    (Point2D.meters -1. 2.)
(***)

Frame2D.withAngle
    (Angle.degrees 20.)
    (Point2D.meters 2. 3.)
(***)

let axis : Axis2D<Meters, Cartesian> =
    Axis2D.through
        (Point2D.meters 3. 2.)
        (Direction2D.fromAngle Angle.pi)
(***)

Frame2D.fromXAxis axis
(***)

Frame2D.fromYAxis axis
(***)

(**
# Accessors
*)

let frame : Frame2D<Meters, Cartesian, unit> =
    Frame2D.withAngle
        (Angle.degrees 20.)
        (Point2D.meters 2. 3.)
(***)

Frame2D.originPoint frame // or

frame.Origin
(*** include-it ***)

Frame2D.xDirection frame // or

frame.XDirection
(*** include-it ***)

Frame2D.yDirection frame // or

frame.YDirection
(*** include-it ***)

Frame2D.isRightHanded frame
(*** include-it ***)

Frame2D.yAxis frame
(*** include-it ***)

Frame2D.xAxis frame
(*** include-it ***)

(**
# Modifiers
*)

Frame2D.reverseX frame
(***)

Frame2D.reverseY frame
(***)

Frame2D.moveTo Point2D.origin frame
(***)

Frame2D.rotateBy (Angle.degrees 30.) frame
(***)

Frame2D.rotateAround
    (Point2D.meters -3. -2.)
    (Angle.degrees 70.)
    frame
(***)

Frame2D.translateBy (Vector2D.meters 4. 7.) frame
(***)

Frame2D.translateIn
(***)

Frame2D.translateAlongOwn
(***)

Frame2D.mirrorAcross
(***)

Frame2D.relativeTo
(***)

Frame2D.placeIn
(***)
