(**

---
title: 2D Arc
category: 2D Modules
categoryindex: 3
index: 1
---

# Arc 2D

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"

#r "../Units/bin/Debug/net6.0/Units.dll"
#r "../Units/bin/Release/net6.0/Units.dll"

open Geometry
open Units
type Cartesian = Cartesian
(***)

(**
# Builders
*)

Arc2D.from
    (Point2D.meters 1. 3.)
    (Point2D.meters 5. 7.)
    (Angle.degrees 30.)
    
(***)

Arc2D.withCenterPoint
    (Point2D.meters 5. 5.)
    (Length.meters 3.)
    (Angle.degrees 30.)
    (Angle.degrees 120.)

(***)

Arc2D.sweptAround
    (Point2D.meters 0. 0.)
    (Angle.degrees 180.)
    (Point2D.meters 5. 0.)

(***)

Arc2D.throughPoints
    (Point2D.meters 5. 0.)
    (Point2D.meters 3. 0.)
    (Point2D.meters -5. 0.)

(***)

Arc2D.withRadius
    (Length.meters 5.)
    SweptAngle.largePositive
    (Point2D.meters 5. 0.)
    (Point2D.meters -5. 0.)
    
(***)

Arc2D.withSweptAngle
    (Point2D.meters 0. 0.)
    (Length.meters 5.)
    Angle.zero
    (Angle.degrees 180.)

(***)

(**
# Accessors

For showing what the different accessors will return, the following arc will be used.
*)

let arc: Arc2D<Meters, Cartesian> = 
    Arc2D.from
        (Point2D.meters 1. 3.)
        (Point2D.meters 5. 7.)
        (Angle.degrees 30.)
        
(***)

Arc2D.centerPoint arc
(*** include-it ***)

Arc2D.radius arc
(*** include-it ***)

Arc2D.sweptAngle arc
(*** include-it ***)

Arc2D.pointOn arc 0.3
(*** include-it ***)

Arc2D.startPoint arc
(*** include-it ***)

Arc2D.midpoint arc
(*** include-it ***)

Arc2D.endPoint arc
(*** include-it ***)


Arc2D.boundingBox arc
(*** include-it ***)

Arc2D.firstDerivative arc 0.8 
(*** include-it ***)


(**
# Modifiers
*)

Arc2D.reverse
Arc2D.scaleAbout
Arc2D.rotateAround
Arc2D.translateBy
Arc2D.translateIn
Arc2D.mirrorAcross
Arc2D.relativeTo
Arc2D.placeIn

(**
# Non-Degenerative
*)

(*** hide ***)
let nondegenerate = arc

(***)

Arc2D.nondegenerate arc
(*** include-it ***)

Arc2D.fromNondegenerate nondegenerate

(***)

Arc2D.tangentDirection nondegenerate 0.2
(*** include-it ***)

Arc2D.sample nondegenerate 0.4
(*** include-it ***)
