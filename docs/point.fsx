(**

---
title: 2D Points
---


*)
(*** hide ***)

#r "../Geometry/bin/Debug/net6.0/Geometry.dll"
open Geometry
(***)

(**
# Constructors

Points can be created from the __x__ and __y__ components.
*)

Point2D.xy (Length.meters 3.) (Length.meters 4.)

(**
You can use either `rTheta` or `polar` when using polar coordinates.
*)
Point2D.rTheta (Length.meters 5.) Angle.halfPi

Point2D.polar (Length.meters 5.) Angle.halfPi



(**
# Operators

Operators are provided for more concise syntax when dealing with points. All
of these symbol operators have a corresponding function that you can use as
well. These are often used when using functions like `List.map` or when used
in piping operations (`|>`). Pay attention to the order of the arguments!
Because these functions are intended to be used in pipes the order goes like
`Point2D.add rhs lhs`.
*)


(*** hide ***)
type Cartesian = Cartesian

let point : Point2D<Meters, Cartesian> = Point2D.meters 3. 4.
let secondPoint : Point2D<Meters, Cartesian> = Point2D.meters 8. 16.
let vec : Vector2D<Meters, Cartesian> = Vector2D.meters 5. 12.

(**
The following three statements are equivalent.
$$ Point2D + Point2D = Point2D $$
*)
Point2D.translate vec point

point |> Point2D.plus vec

point + vec
(*** include-it ***)


(** $$ Point2D - Vector2D = Point2D $$ *)
point - vec
(*** include-it ***)


(** $$ Point2D - Point2D = Vector2D $$ *)
(secondPoint - point) = vec
(*** include-it ***)
(**

The following table is the complete list of operators that can be used on
point objects.

| Operator | Lhs    | Rhs    | Return Type | Example     | Function |
|----------|--------|--------|-------------|-------------|----------|
| -        | Point  |        | Point      | `-vec`      | `Point2D.neg` |
| +        | Point  | Point  | Point      | `lhs + rhs` | `Point2D.plus` |
| -        | Point  | Point  | Point      | `lhs - rhs` | `Point2D.minus` |
| *        | Point  | float  | Point      | `lhs * 0.5` | `Point2D.times` & `Point2D.scaleBy` |
| *        | float  | Point  | Point      | `0.5 * rhs` | `None` |
| /        | Point  | float  | Point      | `lhs / 4.`  | `Point2D.dividedBy` |

*)

