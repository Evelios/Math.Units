(**

---
title: Quantity
category: Tutorials
categoryindex: 1
index: 4
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
#r "../Geometry/bin/Release/net6.0/Geometry.dll"
open Geometry
type Cartesian = Cartesian
(***)

(**
# Operators

| Operator | Lhs    | Rhs    | Return Type | Example          | Function |
|----------|--------|--------|-------------|------------------|----------|
| -        | Angle  |        | Angle       | `-length`        | `Angle.neg` |
| +        | Angle  | Angle  | Angle       | `lhs + rhs`      | `Angle.plus` |
| -        | Angle  | Angle  | Angle       | `lhs - rhs`      | `Angle.minus` |
| *        | Angle  | float  | Angle       | `lhs * 0.5`      | `Angle.times` |
| *        | float  | Angle  | Angle       | `0.5 * rhs`      | None |
| *        | Angle  | Length | Length      | `angle / length` | None |
| *        | Length | Angle  | Length      | `length / angle` | None |
| /        | Angle  | float  | Angle       | `lhs / 4.`       | `Angle.dividedBy` |
| /        | Angle  | Angle  | float       | `lhs / rhs`      | None |
| /        | Angle  | Length | Length      | `angle / length` | None |
| /        | Length | Angle  | Length      | `length / angle` | None |
*)
