module GeometryTests.Vector2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Units
open UnitsTests

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Operators ----

let ``Vector equality test cases`` =
    [ (Vector2D.meters 0. 0., Vector2D.meters 0. 0.)
      (Vector2D.meters -1. -1., Vector2D.meters -1. -1.)
      (Vector2D.meters 5. 5., Vector2D.meters 5. 5.)
      (Vector2D.meters 1. 1., Vector2D.meters (1. + Float.Epsilon / 2.) (1. + Float.Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Vector equality test cases``)>]
let ``Vectors are equal`` (lhs: Vector2D<Meters, 'Coordinates>) (rhs: Vector2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

let ``Vector less than test cases`` =
    [ (Vector2D.meters 0. 0., Vector2D.meters 1. 0.)
      (Vector2D.meters 1. 0., Vector2D.meters 1. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Vector less than test cases``)>]
let ``Vectors are less than`` (lhs: Vector2D<Meters, 'Coordinates>) (rhs: Vector2D<Meters, 'Coordinates>) =
    Assert.Less(lhs, rhs)

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


// ---- Accessors -----

[<Test>]
let Magnitude () =
    let vector = Vector2D.meters 2. 2.
    Assert.AreEqual(Length.meters (2. * sqrt 2.), Vector2D.magnitude vector)

[<Test>]
let Direction () =
    let vector = Vector2D.meters 1. 1.
    let actual = Vector2D.direction vector

    let expected =
        Direction2D.xy (sqrt 2.) (sqrt 2.)

    Assert.AreEqual(expected, actual)

// ---- Modifiers ----

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

    let expected =
        Vector2D.meters (sqrt 2.) (sqrt 2.)

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

    let actual =
        Vector2D.round (Vector2D.meters 22.2222222222 22.2222222222)

    let expected =
        (Vector2D.meters 22.22222222 22.22222222)

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

    let actual: Quantity<Meters Squared> =
        Vector2D.distanceSquaredTo v1 v2

    let expected: Quantity<Meters Squared> =
        Quantity 8.

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
let ``From list`` () =
    let actual: Vector2D<Meters, TestSpace> option =
        Vector2D.fromList [ 1.; 2. ]

    let expected: Vector2D<Meters, TestSpace> option =
        Some(Vector2D.meters 1. 2.)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``To list`` () =
    let actual =
        Vector2D.toList (Vector2D.meters 1. 2.)

    let expected = [ 1.; 2. ]
    Assert.AreEqual(expected, actual)

[<Property>]
let ``Dot product of a vector with itself is the length squared`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Length.squared (Vector2D.magnitude vector)) (Vector2D.dot vector vector)

[<Property>]
let ``Normalized vector has a magnitude of one`` (vector: Vector2D<Meters, TestSpace>) =
    Test.equal (Area.create 1.) (vector |> Vector2D.normalize |> Vector2D.magnitude)

[<Property>]
let ``Perpendicular vector is perpendicular`` (vector: Vector2D<Meters, TestSpace>) =
    vector
    |> Vector2D.perpendicularTo
    |> Vector2D.dot vector
    |> Test.equal Quantity.zero

[<Property>]
let ``Dot product of a vector with itself is it's squared length`` (vector: Vector2D<Meters, TestSpace>) =
    vector
    |> Vector2D.dot vector
    |> Test.equal (Length.squared (Vector2D.magnitude vector))

[<Property>]
let ``Rotate by preserves length`` (vector: Vector2D<Meters, TestSpace>) (angle: Angle) =
    vector
    |> Vector2D.rotateBy angle
    |> Vector2D.magnitude
    |> Test.equal (Vector2D.magnitude vector)

[<Property>]
let ``Rotating rotates correct angle`` (vector: Vector2D<Meters, TestSpace>) (angle: Angle) =
    let direction = Vector2D.direction vector

    let rotatedDirection =
        Vector2D.rotateBy angle vector
        |> Vector2D.direction

    let measuredAngle =
        Option.map2 Direction2D.angleFrom direction rotatedDirection
        |> Option.defaultValue Angle.zero


    Test.equal angle measuredAngle

[<Property>]
let ``Components return equal values`` (vector: Vector2D<Meters, TestSpace>) =
    Vector2D.components vector = (Vector2D.x vector, Vector2D.y vector)

[<Property>]
let ``Accessors equal components`` (vector: Vector2D<Meters, TestSpace>) =
    (vector.X, vector.Y) = (Vector2D.x vector, Vector2D.y vector)

[<Property>]
let ``Scaling zero length vector returns zero`` (length: Length) =
    Test.equal Vector2D.zero (Vector2D.scaleTo length Vector2D.zero)

[<Property>]
let ``Scale to returns consistent length`` (vector: Vector2D<Meters, TestSpace>) (scale: Length) =
    if vector = Vector2D.zero then
        Test.equal Vector2D.zero (Vector2D.scaleTo scale vector)

    else
        Vector2D.scaleTo scale vector
        |> Vector2D.length
        |> Test.equal (Length.abs scale)

[<Property>]
let ``Normalize has a consistent length`` (vector: Vector2D<Meters, TestSpace>) =
    Vector2D.normalize vector
    |> Vector2D.length
    |> Test.equal (Length.meters 1.)
