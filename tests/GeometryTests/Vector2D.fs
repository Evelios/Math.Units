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
    (first: Vector2D<Meters, TestSpace>)
    (second: Vector2D<Meters, TestSpace>)
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
    Assert.AreEqual(Length.meters (2. * sqrt 2.), Vector2D.magnitude vector)

[<Test>]
let Scale () =
    let actual =
        Vector2D.scaleBy 2. (Vector2D.meters 2. 2.)

    let expected = Vector2D.meters 4. 4.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Scale to`` () =
    let actual =
        Vector2D.scaleTo (Length.meters 2.) (Vector2D.meters 2. 2.)

    let expected = Vector2D.meters (sqrt 2.) (sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rotate counterclockwise`` () =
    let actual =
        Vector2D.rotateBy Angle.halfPi (Vector2D.meters 2. 2.)

    let expected = (Vector2D.meters -2. 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rotate clockwise`` () =
    let actual =
        Vector2D.rotateBy (-Angle.halfPi) (Vector2D.meters 2. 2.)

    let expected = (Vector2D.meters 2. -2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let Round () =
    Float.DigitPrecision <- 8
    let actual = Vector2D.round (Vector2D.meters 22.2222222222 22.2222222222)
    let expected = (Vector2D.meters 22.22222222 22.22222222)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Round to`` () =
    let actual =
        Vector2D.roundTo 2 (Vector2D.meters 2.222 2.222)

    let expected = (Vector2D.meters 2.22 2.22)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Distance squared to`` () =
    let v1 = Vector2D.meters 1. 1.
    let v2 = Vector2D.meters 3. 3.
    let actual : Length<Meters * Meters> = Vector2D.distanceSquaredTo v1 v2
    let expected : Length<Meters * Meters> = Length<Meters * Meters>.create 8.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Distance to`` () =
    let v1 = Vector2D.meters 1. 1.
    let v2 = Vector2D.meters 3. 3.
    let actual = Vector2D.distanceTo v1 v2
    let expected = Length.meters (2. * sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Mid vector`` () =
    let v1 = Vector2D.meters 1. 1.
    let v2 = Vector2D.meters 3. 3.
    let actual = Vector2D.midVector v1 v2
    let expected = Vector2D.meters 2. 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Direction () =
    let vector = Vector2D.meters 1. 1.
    let actual = Vector2D.direction vector
    let expected = Direction2D.xy (sqrt 2.) (sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``From list`` () =
    let actual : Vector2D<Meters, TestSpace> option = Vector2D.fromList [ 1.; 2. ]
    let expected : Vector2D<Meters, TestSpace> option = Some(Vector2D.meters 1. 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``To list`` () =
    let actual = Vector2D.toList (Vector2D.meters 1. 2.)
    let expected = [ 1.; 2. ]
    Assert.AreEqual(expected, actual)

[<Property>]
let ``Dot product of a vector with itself is the length squared`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Length.square (Vector2D.magnitude vector)) (Vector2D.dotProduct vector vector)

[<Property>]
let ``Normalized vector has a magnitude of one`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Length<Meters * Meters>.create 1.) (vector |> Vector2D.normalize |> Vector2D.magnitude)
