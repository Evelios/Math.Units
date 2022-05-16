(**

---
title: 2D Size
category: 2D Modules
categoryindex: 3
index: 12
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian
(***)

(**
# Builders
*)

(**
Create a size with zero length and zero height.
*)

Size2D.empty
(*** include-it ***)

Size2D.create (Length.meters 4.) (Length.meters 3.)

(***)

(**
# Modifiers
*)

let size = Size2D.create (Length.meters 2.) (Length.meters 7.)

(***)

Size2D.scale 1.5 size
(*** include-it ***)

Size2D.normalizeBelowOne size
(*** include-it ***)

Size2D.withMaxSize (Length.meters 14.) size
(*** include-it ***)
