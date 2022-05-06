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


(**
# Operators

Operators are provided for more concise syntax when dealing with vectors. All
of these symbol operators have a corresponding function that you can use as
well. These are often used when using functions like `List.map` or when used
in piping operations (`|>`). Pay attention to the order of the arguments!
Because these functions are intended to be used in pipes the order goes like
`Vector2D.add rhs lhs`.
*)


(*** hide ***)
type Cartesian = Cartesian

let lhs : Vector2D<Meters, Cartesian> = Vector2D.meters 3. 4.
let rhs : Vector2D<Meters, Cartesian> = Vector2D.meters 5. 12.
(***)

lhs |> Vector2D.plus rhs = lhs + rhs
(*** include-it ***)
(**

The following table is the complete list of operators that can be used on
vector objects.

| Operator | Lhs    | Rhs    | Return Type | Example     | Function |
|----------|--------|--------|-------------|-------------|----------|
| -        | Vector |        | Vector      | `-vec`      | `Vector2D.neg` |
| +        | Vector | Vector | Vector      | `lhs + rhs` | `Vector2D.plus` |
| -        | Vector | Vector | Vector      | `lhs - rhs` | `Vector2D.minus` |
| *        | Vector | float  | Vector      | `lhs * 0.5` | `Vector2D.times` & `Vector2D.scaleBy` |
| *        | float  | Vector | Vector      | `0.5 * rhs` | `None` |
| /        | Vector | float  | Vector      | `lhs / 4.`  | `Vector2D.dividedBy` |


# Trigonometry

*)

Vector2D.cross lhs rhs
(*** include-it ***)

Vector2D.cross lhs rhs
(*** include-it ***)

