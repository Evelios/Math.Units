(**

---
title: 2D Direction
category: 2D Modules
categoryindex: 3
index: 5
---


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
# Direction 2D

A direction can be though of as a normalized vector or can be though of just something pointing in a particular
direction on the 2D cartesian plane. When looking at the type of a direction, you can notice that it doesn't have any
unit type, `Direction2D<'Coordinates>`. This is because a direction _always_ has a magnitude of 1 and doesn't need any
unit qualifier.
*)

(** # Builders *)

(**
`Direction2D.xy` first checks to make sure that the input components are both non-zero, then normalizes them to
guarantee a valid direction is created.
*)

Direction2D.xy 3. 4.
(***hide***)
Direction2D.xy 3. 4.
|> Option.map Direction2D.toTuple
(*** include-it ***)

(**
When both values are zero, a proper direction cannot be created, so the program returns a `None` option.
(Appears as `<null>` in the terminal)
*)

Direction2D.xy 0. 0.
(*** hide ***)
Direction2D.xy 0. 0.
|> Option.map Direction2D.toTuple
(*** include-it ***)

(**
You can also create a direction using similar functionality to `Direction2D.xy` but with _tuples_ as an input instead.
*)
Direction2D.fromComponents (3., 4.)
(*** hide ***)


Direction2D.positiveY |> Direction2D.toAngle
(*** include-it ***)

(** # Constants *)

Direction2D.positiveX
Direction2D.x

Direction2D.positiveY
Direction2D.y

Direction2D.negativeX

Direction2D.negativeY

(** # Accessors *)

(*** hide ***)
let direction: Direction2D<Cartesian> = Direction2D.xyUnsafe 1. 0.5

(***)

Direction2D.toAngle
Direction2D.toTuple

Direction2D.xComponent
direction.X

Direction2D.yComponent
direction.Y


(** # Modifiers *)

Direction2D.reverse
Direction2D.rotateClockwise
Direction2D.rotateCounterclockwise
Direction2D.perpendicularTo
Direction2D.mirrorAcross
Direction2D.orthonormalize

(** # Queries *)
Direction2D.equalWithin
Direction2D.componentIn
Direction2D.angleFrom

(** # Coordinate Conversions *)
Direction2D.relativeTo
Direction2D.placeIn
