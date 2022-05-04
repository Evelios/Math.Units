(**

---
title: 2D Vectors
---


*)

(*** hide ***)
#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
(***)

(**
# Constructors

Vectors can be created from the __x__ and __y__ components.
*)

Vector2D.xy (Length.meters 3.) (Length.meters 4.)

(**
You can also create a vector in polar coordinates.
*)
Vector2D.rTheta (Length.meters 5.) Angle.halfPi

(**
Using a vector __direction__ you can create a vector of a given length following that direction.
*)
Vector2D.withLength (Length.meters 5.) (Direction2D.fromAngle Angle.halfPi)
