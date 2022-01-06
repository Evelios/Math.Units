module GeometryTests.Vector2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Vector from polar`` () =
    let expected = Vector2D.xy 0. 1.
    let actual = Vector2D.ofPolar 1. (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)

[<Property>]
let ``Equality and hash code comparison with random points`` (first: Vector2D<'Length, 'Coordinates>) (second: Vector2D<'Length, 'Coordinates>) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

[<Property>]
let ``Dot product of a vector with itself is the length squared`` (vector: Vector2D<'Length, 'Coordinates>) =
    Test.almostEqual ((Vector2D.magnitude vector) ** 2.) (Vector2D.dotProduct vector vector)

[<Property>]
let ``Normalized vector has a magnitude of one`` (vector: Vector2D<'Length, 'Coordinates>) =
    Test.almostEqual 1. (vector |> Vector2D.normalize |> Vector2D.magnitude)
