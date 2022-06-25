(**

---
title: FSharp Geometry Package
---

This package is intended to be a port and extension of the framework [elm-geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/).
It focuses on providing as many interfaces for geometric objects an manipulation in a way that is type safe and
convenient.

This framework is currently in alpha development and is currently working on building out 2D development and
functionality first before moving to expand into 3D objects.

*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
(** *)

(**
To use this framework you include the package through the namespace
*)
open Geometry

(**
You can then create a variety of geometric objects like points, vectors, angles, ...
*)

Point2D.pixels 100. 200.

Vector2D.meters 4. 4.

Angle.degrees 30.
(**

# Overview
| Tutorial | Api |
|----------|-----|
| [Vector](vector.html) | [2D](reference/geometry-vector2dmodule.html) |
| [Point](point.html) | [2D](reference/geometry-point2dmodule.html) |
| [Direction](direction.html) | [2D](reference/geometry-direction2dmodule.html) |
| [Frame](frame.html) | [2D](reference/geometry-frame2dmodule.html) |
| [Axes](axes.html) | [2D](reference/geometry-axes2dmodule.html) |
| [Line Segment](line-segment.html) | [2D](reference/geometry-linesegment2dmodule.html) |
| [Triangle](triangle.html) | [2D](reference/geometry-triangle2dmodule.html) |
| [Circle](circle.html) | [2D](reference/geometry-circle2dmodule.html) |
| [Ellipse](ellipse.html) | [2D](reference/geometry-ellipse2dmodule.html) |
| [Arc](arc.html) | [2D](reference/geometry-arc2dmodule.html) |
| [Polyline](polyline.html) | [2D](reference/geometry-polyline2dmodule.html) |
| [Polygon](polygon.html) | [2D](reference/geometry-polygon2dmodule.html) |
| [Size ](size.html) | [2D](reference/geometry-size2dmodule.html) |
| [Bounding Box](bounding-box.html) | [2D](reference/geometry-boundingbox2dmodule.html) |

*)
