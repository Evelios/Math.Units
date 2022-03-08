module GeometryTests.Intersection2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Empty Test`` () = Assert.Pass()
