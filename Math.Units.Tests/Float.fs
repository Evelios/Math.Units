module Math.Units.Tests.Float

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Math.Units

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Floating point rounding`` () =
    Assert.AreEqual(2.0, Float.roundFloatTo 10 2.00000000003)

[<Property>]
let ``Round to float`` (f: float) =
    Float.roundFloatTo Float.DigitPrecision f
    |> Float.almostEqual (Float.roundFloat f)


[<Property>]
let ``Interpolation gives start point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 0. .=. start

[<Property>]
let ``Interpolation gives end point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 1. .=. finish

[<Test>]
let ``Interpolation gives midpoint`` () =
    Assert.AreEqual(Float.interpolateFrom 10. 20. 0.5, 15.)
