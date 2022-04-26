module GeometryTests.Float

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Property>]
let ``Interpolation gives start point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 0. .=. start

[<Property>]
let ``Interpolation gives end point`` (start: float) (finish: float) =
    Float.interpolateFrom start finish 1. .=. finish

[<Test>]
let ``Interpolation gives midpoint`` () =
    Assert.AreEqual(Float.interpolateFrom 10. 20. 0.5, 15.)
