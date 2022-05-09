(**

---
title: Angle
category: Modules
categoryindex: 2
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"

open Geometry
open System

type Cartesian = Cartesian
(***)

(**
# Builders
*)

Angle.degrees 120.

Angle.radians (Math.PI / 3.)

(**
# Accessors
*)

Angle.inDegrees Angle.halfPi

Angle.inRadians Angle.halfPi

(**
# Trigonometry
*)

Angle.sin Angle.pi
Angle.cos Angle.pi
Angle.tan Angle.pi

Angle.asin (1. / 2.)
Angle.acos (1. / 2.)
Angle.atan (1. / 2.)

(**
# Constants

| Function                 | Value               |
|--------------------------|---------------------|
| `Angle.zero`             | 0                   |
| `Angle.pi`               | $ \pi $             |
| `Angle.twoPi`            | $ 2 \pi $           |
| `Angle.piOverTwo`        | $ \frac{\pi}{2} $   |
| `Angle.halfPi`           | $ \frac{\pi}{2} $   |
| `Angle.radiansToDegrees` | $ \frac{180}{\pi} $ |
| `Angle.degreesToRadians` | $ \frac{\pi}{180} $ |

*)

