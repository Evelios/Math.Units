(**

---
title: 2D Ellipse
category: 2D Modules
categoryindex: 3
index: 6
---

# Ellipse 2D


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian

(**
# Builders
*)

Ellipse2D.from
    (Point2D.meters 4. 4.)
    Direction2D.x
    (Length.meters 2.)
    (Length.meters 4.)
(*** include-it ***)

(**
# Accessors
*)
let ellipse: Ellipse2D<Meters, Cartesian> =
    Ellipse2D.from
        (Point2D.meters 4. 4.)
        Direction2D.x
        (Length.meters 2.)
        (Length.meters 4.)

(***)

Ellipse2D.axes ellipse  // or
ellipse.Axes
(*** include-it ***)

Ellipse2D.xRadius ellipse  // or
ellipse.XRadius
(*** include-it ***)

Ellipse2D.yRadius ellipse  // or
ellipse.YRadius
(*** include-it ***)

Ellipse2D.centerPoint ellipse
(*** include-it ***)

Ellipse2D.xAxis ellipse
(*** include-it ***)

Ellipse2D.yAxis ellipse
(*** include-it ***)

Ellipse2D.xDirection ellipse
(*** include-it ***)

Ellipse2D.yDirection ellipse
(*** include-it ***)

Ellipse2D.area ellipse
(*** include-it ***)


(**
# Modifiers
*)

Ellipse2D.scaleAbout
Ellipse2D.transformBy
Ellipse2D.rotateAround
Ellipse2D.translateBy
Ellipse2D.translateIn
Ellipse2D.relativeTo
Ellipse2D.placeIn
