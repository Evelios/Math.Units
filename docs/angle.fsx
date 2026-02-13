(**
---
title: Angle
category: Modules
categoryindex: 2
index: 1
---
*)
(*** hide ***)

#I "../Math.Units/bin/Release/net8.0"
#I "../Math.Units/bin/Debug/net8.0"
#I "../Math.Units/bin/Release/net9.0"
#I "../Math.Units/bin/Debug/net9.0"
#I "../Math.Units/bin/Release/net10.0"
#I "../Math.Units/bin/Debug/net10.0"
#r "Math.Units.dll"

open Math.Units

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

(**
# Operators

| Operator | Lhs    | Rhs    | Return Type | Example          | Function |
|----------|--------|--------|-------------|------------------|----------|
| -        | Angle  |        | Angle       | `-length`        | `Quantity.negate` |
| +        | Angle  | Angle  | Angle       | `lhs + rhs`      | `Quantity.plus` |
| -        | Angle  | Angle  | Angle       | `lhs - rhs`      | `Quantity.minus` |
| *        | Angle  | float  | Angle       | `lhs * 0.5`      | `Quantity.multiplyBy` |
| *        | float  | Angle  | Angle       | `0.5 * rhs`      | `Quantity.multiplyBy` |
| *        | Angle  | Length | Length      | `angle / length` | None |
| *        | Length | Angle  | Length      | `length / angle` | None |
| /        | Angle  | float  | Angle       | `lhs / 4.`       | `Quantity.divideBy` |
| /        | Angle  | Angle  | float       | `lhs / rhs`      | `Quantity.ratio` |
| /        | Angle  | Length | Length      | `angle / length` | None |
| /        | Length | Angle  | Length      | `length / angle` | None |
*)
