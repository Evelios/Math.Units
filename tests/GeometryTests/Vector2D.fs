module GeometryTests.Vector2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

let vectorEqualityTestCases =
    [ (Vector2D.xy 0. 0., Vector2D.xy 0. 0.)
      (Vector2D.xy -1. -1., Vector2D.xy -1. -1.)
      (Vector2D.xy 5. 5., Vector2D.xy 5. 5.)
      (Vector2D.xy 1. 1., Vector2D.xy (1. + Epsilon / 2.) (1. + Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof vectorEqualityTestCases)>]
let ``Vectors are equal`` (lhs: Vector2D<float, TestSpace>) (rhs: Vector2D<float, TestSpace>) =
    Assert.AreEqual(lhs, rhs)
    Assert.That(lhs.Equals(rhs))

[<Test>]
let ``Vector from polar`` () =
    let expected = Vector2D.xy 0. 1.
    let actual = Vector2D.ofPolar 1. (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)

[<Property>]
let ``Equality and hash code comparison with random points`` (first: Vector2D<float, TestSpace>) (second: Vector2D<float, TestSpace>) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

[<Property>]
let ``Dot product of a vector with itself is the length squared`` (vector: Vector2D<float, TestSpace>) =
    Test.almostEqual ((Vector2D.magnitude vector) ** 2.) (Vector2D.dotProduct vector vector)

[<Property>]
let ``Normalized vector has a magnitude of one`` (vector: Vector2D<float, TestSpace>) =
    Test.almostEqual 1. (vector |> Vector2D.normalize |> Vector2D.magnitude)
