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

#r "../Geometry/bin/Debug/net5.0/Geometry.dll"

open Geometry

Point2D.pixels 100. 200.

Vector2D.meters 4. 4.

