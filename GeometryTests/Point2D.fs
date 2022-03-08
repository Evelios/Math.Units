module GeometryTests.Point2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Operators ----

let ``Point equality test cases`` =
    [ Point2D.meters 0. 0., Point2D.meters 0. 0.
      Point2D.meters -1. -1., Point2D.meters -1. -1.
      Point2D.meters 5. 5., Point2D.meters 5. 5.
      Point2D.meters 1. 1., Point2D.meters (1. + Epsilon / 2.) (1. + Epsilon / 2.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Point equality test cases``)>]
let ``Points are equal`` (lhs: Point2D<Meters, 'Coordinates>) (rhs: Point2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

let ``Point less than test cases`` =
    [ (Point2D.meters 0. 0., Point2D.meters 1. 0.)
      (Point2D.meters 1. 0., Point2D.meters 1. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Point equality test cases``)>]
let ``Points are less than`` (lhs: Point2D<Meters, 'Coordinates>) (rhs: Point2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

[<Property>]
let ``Equality and hash code comparison with random points``
    (first: Point2D<Meters, TestSpace>)
    (second: Point2D<Meters, TestSpace>)
    =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

// ---- Builders ----

[<Test>]
let ``Point from polar`` () =
    let expected = Point2D.meters 0. 1.

    let actual =
        Point2D.polar (Length.meters 1.) (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)

// ---- Other Tests -----

[<Test>]
let Magnitude () =
    let vector = Point2D.meters 2. 2.
    Assert.AreEqual(Length.meters (2. * sqrt 2.), Point2D.magnitude vector)

[<Test>]
let Scale () =
    let actual =
        Point2D.scaleBy 2. (Point2D.meters 2. 2.)

    let expected = Point2D.meters 4. 4.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Scale to`` () =
    let actual =
        Point2D.scaleTo (Length.meters 2.) (Point2D.meters 2. 2.)

    let expected = Point2D.meters (sqrt 2.) (sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rotate counterclockwise`` () =
    let actual =
        Point2D.rotateBy Angle.halfPi (Point2D.meters 2. 2.)

    let expected = (Point2D.meters -2. 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rotate clockwise`` () =
    let actual =
        Point2D.rotateBy (-Angle.halfPi) (Point2D.meters 2. 2.)

    let expected = (Point2D.meters 2. -2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let Round () =
    Float.DigitPrecision <- 8

    let actual =
        Point2D.round (Point2D.meters 22.2222222222 22.2222222222)

    let expected = (Point2D.meters 22.22222222 22.22222222)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Round to`` () =
    let actual =
        Point2D.roundTo 2 (Point2D.meters 2.222 2.222)

    let expected = (Point2D.meters 2.22 2.22)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Distance squared to`` () =
    let v1 = Point2D.meters 1. 1.
    let v2 = Point2D.meters 3. 3.
    let actual : Length<Meters * Meters> = Point2D.distanceSquaredTo v1 v2
    let expected : Length<Meters * Meters> = Length<Meters * Meters>.create 8.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Distance to`` () =
    let v1 = Point2D.meters 1. 1.
    let v2 = Point2D.meters 3. 3.
    let actual = Point2D.distanceTo v1 v2
    let expected = Length.meters (2. * sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Mid vector`` () =
    let v1 = Point2D.meters 1. 1.
    let v2 = Point2D.meters 3. 3.
    let actual = Point2D.midpoint v1 v2
    let expected = Point2D.meters 2. 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Direction () =
    let vector = Point2D.meters 1. 1.
    let actual = Point2D.direction vector
    let expected = Direction2D.xy (sqrt 2.) (sqrt 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``From list`` () =
    let actual : Point2D<Meters, TestSpace> option = Point2D.fromList [ 1.; 2. ]
    let expected : Point2D<Meters, TestSpace> option = Some(Point2D.meters 1. 2.)
    Assert.AreEqual(expected, actual)

[<Test>]
let ``To list`` () =
    let actual = Point2D.toList (Point2D.meters 1. 2.)
    let expected = [ 1.; 2. ]
    Assert.AreEqual(expected, actual)
