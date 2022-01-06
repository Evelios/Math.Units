module GeometryTests.Point2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


let pointEqualityTestCases =
    [ (Point2D.xy 0. 0., Point2D.xy 0. 0.)
      (Point2D.xy -1. -1., Point2D.xy -1. -1.)
      (Point2D.xy 5. 5., Point2D.xy 5. 5.)
      (Point2D.xy 1. 1., Point2D.xy (1. + Epsilon / 2.) (1. + Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointEqualityTestCases)>]
let ``Points are equal`` (lhs: Point2D<Meters, TestSpace>) (rhs: Point2D<Meters, TestSpace>) =
//    Assert.AreEqual(lhs, rhs)
    Assert.That(lhs.Equals(rhs))

[<Test>]
let ``Point from polar`` () =
    let expected = Point2D.xy 0. 1.
    let actual = Point2D.ofPolar 1. (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)



let pointLessThanTestCases =
    [ (Point2D.xy 0. 0., Point2D.xy 1. 1.)
      (Point2D.xy 0. 0., Point2D.xy 0. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointLessThanTestCases)>]
let ``Point less than`` (lhs: Point2D<Meters, TestSpace>) (rhs: Point2D<Meters, TestSpace>) = Assert.Less(lhs, rhs)

[<Property>]
let ``Equality and hash code comparison with random points``
    (first: Point2D<Meters, TestSpace>)
    (second: Point2D<Meters, TestSpace>)
    =
    (first = second) = (first.GetHashCode() = second.GetHashCode())
