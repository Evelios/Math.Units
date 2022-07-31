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

#r "../Units/bin/Debug/net6.0/Units.dll"
#r "../Units/bin/Release/net6.0/Units.dll"

open Geometry
open Units

type Cartesian = Cartesian
(***)

(**
# Operators

| Operator | Lhs       | Rhs       | Return Type | Example          | Function |
|----------|-----------|-----------|-------------|------------------|----------|
| -        | Quantity  |           | Quantity    | `-quantity`      | `Quantity.negate` |
| +        | Quantity  | Quantity  | Quantity    | `lhs + rhs`      | `Quantity.plus` |
| -        | Quantity  | Quantity  | Quantity    | `lhs - rhs`      | `Quantity.difference` |
| *        | Quantity  | Quantity  | Quantity    | `lhs * rhs`      | `Quantity.product` & `Quantity.times` |
| *        | Quantity  | float     | Quantity    | `lhs * 0.5`      | `Quantity.multiplyBy` |
| *        | float     | Quantity  | Quantity    | `0.5 * rhs`      | `Quantity.multiplyBy` |
| /        | Quantity  | Quantity  | float       | `lhs / rhs`      | `Quantity.ratio` |
| /        | Quantity  | float     | Quantity    | `lhs / 4.`       | `Quantity.dividedBy` |

*)

(**

| Operator | Lhs       | Return Type | Example          | Function |
|----------|-----------|-------------|------------------|----------|
| abs      | Quantity  | Quantity    | `-length`        | `Quantity.abs` |
| min      | Quantity  | Quantity    | `lhs + rhs`      | `Quantity.min` |
| max      | Quantity  | Quantity    | `lhs - rhs`      | `Quantity.max` |
| sqrt     | Quantity<'Unit>  | Quantity<'Unit Squared>  | `lhs * 0.5`      | `Quantity.sqrt` |
| floor    | Quantity  | Quantity    | `0.5 * rhs`      | `Quantity.floor` |
| ceil     | Quantity  | Length      | `angle / length` | `Quantity.ceil` |
| round    | Quantity  | Quantity      | `length / angle` | `Quantity.round` |
| truncate | Quantity  | Quantity    | `lhs / 4.`       | `Quantity.truncate` |

*)
