(**

---
title: Lengths
category: Modules
categoryindex: 2
index: 2
---

[digit precision]: digit-precision.html

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


(***)

(*** hide ***)
type Cartesian = Cartesian

(**

# Builders and Accessors (Units)

| Builder                  | Accessor                   | Length                                    |
|--------------------------|----------------------------|-------------------------------------------|
| __Metric__               |                            |                                           |
| Length.angstroms         | Length.inAngstroms         | $$ 1 * 10^{-10} \, Meters $$              |
| Length.nanometers        | Length.inNanometers        | $$ 1 * 10^9 \, Meters $$                  |
| Length.microns           | Length.inMicrons           | $$ 1 * 10^{-6} \, Meters $$               |
| Length.millimeters       | Length.inMillimeters       | $$ 0.001 \, Meters $$                     |
| Length.centimeters       | Length.inCentimeters       | $$ 0.01 \, Meters $$                      |
| Length.kilometers        | Length.inKilometers        | $$ 1000 \, Meters $$                      |
| __Imperial__             |                            |                                           |
| Length.inch              | Length.inInches            | $$ 0.0254 \, Meters $$                    |
| Length.feet              | Length.inFeet              | $$ 12 \, Inches $$                        |
| Length.yards             | Length.inYards             | $$ 3 \, Feet $$                           |
| Length.thou              | Length.inThou              | $$ 0.001 \, Inches $$                     |
| Length.miles             | Length.inMiles             | $$ 5280 \, Feet $$                        |
| __Astronomical Units__   |                            |                                           |
| Length.astronomicalUnits | Length.inAstronomicalUnits | $$ \approx 1.50 * 10^{11} \, Meters $$    |
| Length.lightYears        | Length.inLightYears        | $$ \approx 9.46 * 10^{15} \, Meters $$    |
| Length.parsecs           | Length.inParsecs           | $$ 648000 \pi \, Astronomical \, Units $$ |
| __Digital__              |                            |                                           |
| Length.cssPixels         | Length.inCssPixels         | $$ \frac{1}{96} \, Inches $$              |
| Length.points            | Length.inPoints            | $$ \frac{1}{72} \, Inches $$              |
| Length.picas             | Length.inPicas             | $$ \frac{1}{6} \, Inches $$               |

*)

(**
# Operators

| Operator | Lhs    | Rhs    | Return Type | Example           | Function |
|----------|--------|--------|-------------|-------------      |----------|
| -        | Length |        | Length      | `-length`         | `Quantity.negate` |
| +        | Length | Length | Length      | `lhs + rhs`       | `Quantity.plus` |
| -        | Length | Length | Length      | `lhs - rhs`       | `Quantity.minus` |
| *        | Length | float  | Length      | `lhs * 0.5`       | `Quantity.multiplyBy` |
| *        | float  | Length | Length      | `0.5 * rhs`       | `Quantity.multiplyBy` |
| *        | Angle  | Length | Length      | `angle / length`  | None |
| *        | Length | Angle  | Length      | `length / angle`  | None |
| /        | Length | float  | Length      | `lhs / 4.`        | `Quantity.divideBy` |
| /        | Angle  | Length | Length      | `angle / length`  | None |
| /        | Length | Angle  | Length      | `length / angle`  | None |
*)

(**
# Math
*)

let length = Length.meters 10.

Quantity.squared length
(*** include-it ***)

let lengthSquared = Quantity.product (Length.meters 3.) (Length.meters 12.)

Quantity.sqrt lengthSquared
(*** include-it ***)

Quantity.twice length = (Quantity.multiplyBy 2. length)

Quantity.half length = (Quantity.multiplyBy 0.5 length)
Quantity.half length = (Quantity.divideBy 2. length)

(** Round to the nearest 10th digit. You can read more about [digit precision] *)

Quantity.round length
(*** include-it ***)

Quantity.roundTo 3 (Length.meters 0.11111)
(*** include-it ***)

Quantity.min (Length.meters 3.) (Length.meters 4.)
(*** include-it ***)

Quantity.max (Length.meters 3.) (Length.meters 4.)
(*** include-it ***)

Quantity.sum [ Length.meters 1.; Length.meters 2. ]
(*** include-it ***)


(**
# Unsafe Operations
*)

(*** include-it ***)
