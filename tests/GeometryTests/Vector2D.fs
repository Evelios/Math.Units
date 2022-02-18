module GeometryTests.Vector2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Operators ----

let ``Vector equality test cases`` =
    [ (Vector2D.meters 0. 0., Vector2D.meters 0. 0.)
      (Vector2D.meters -1. -1., Vector2D.meters -1. -1.)
      (Vector2D.meters 5. 5., Vector2D.meters 5. 5.)
      (Vector2D.meters 1. 1., Vector2D.meters (1. + Epsilon / 2.) (1. + Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Vector equality test cases``)>]
let ``Vectors are equal`` (lhs: Vector2D<Meters, 'Coordinates>) (rhs: Vector2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

let ``Vector less than test cases`` =
    [ (Vector2D.meters 0. 0., Vector2D.meters 1. 0.)
      (Vector2D.meters 1. 0., Vector2D.meters 1. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Vector equality test cases``)>]
let ``Vectors are less than`` (lhs: Vector2D<Meters, 'Coordiantes>) (rhs: Vector2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

[<Property>]
let ``Equality and hash code comparison with random points``
    (first: Vector2D<float, TestSpace>)
    (second: Vector2D<float, TestSpace>)
    =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

// ---- Builders ----

[<Test>]
let ``Vector from polar`` () =
    let expected = Vector2D.meters 0. 1.

    let actual =
        Vector2D.polar (Length.meters 1.) (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)

// ---- Other Tests -----

[<Test>]
let Magnitude () =
    let vector = Vector2D.meters 2. 2.
    Assert.Equals(2. * sqrt 2., Vector2D.magnitude vector)
    
[<Test>]
let Scale () =
    let actual = Vector2D.scaleBy 2. (Vector2D.meters 2. 2.)
    let expected = Vector2D.meters 4. 4.
    Assert.Equals(expected, actual)
    
[<Test>]
let ``Scale to`` () =
    let actual = Vector2D.scaleTo (Length.meters 2.) (Vector2D.meters 2. 2.)
    let expected = Vector2D.meters (sqrt 2.) (sqrt 2.)
    Assert.Equals(expected, actual)
    
[<Test>]
let ``Rotate counterclockwise`` () =
    let actual = Vector2D.rotateBy Angle.halfPi (Vector2D.meters 2. 2.)
    let expected = (Vector2D.meters -2. 2.)
    Assert.Equals(expected, actual)


[<Property>]
let ``Dot product of a vector with itself is the length squared`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Length.square (Vector2D.magnitude vector)) (Vector2D.dotProduct vector vector)

[<Property>]
let ``Normalized vector has a magnitude of one`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Length<Meters * Meters>.create 1.) (vector |> Vector2D.normalize |> Vector2D.magnitude)
