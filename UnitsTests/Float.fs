module UnitsTests.Float

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Ignore("Property doesn't always hold. Need a better test here")>]
[<Property>]
let ``Floating numbers close to each other are almost equal to each other`` (number: float) =
    let smallEpsilon =
        Gen.floatBetween -Float.Epsilon Float.Epsilon
        |> Arb.fromGen

    let areEqual e = Float.almostEqual number (number + e)

    Prop.forAll smallEpsilon (fun e -> Test.isTrue $"{number} != {number + e}" (areEqual e))


[<Test>]
let ``Floating point rounding`` () =
    Assert.AreEqual(2.0, Float.roundFloatTo 10 2.00000000003)


[<Property>]
let ``Interpolation gives start point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 0. .=. start

[<Property>]
let ``Interpolation gives end point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 1. .=. finish

[<Test>]
let ``Interpolation gives midpoint`` () =
    Assert.AreEqual(Float.interpolateFrom 10. 20. 0.5, 15.)
