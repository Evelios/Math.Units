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

// ---- Accessors -----

[<Test>]
let Magnitude () =
    let vector = Point2D.meters 2. 2.
    Assert.AreEqual(Length.meters (2. * sqrt 2.), Point2D.magnitude vector)


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

[<Property>]
let ``coordinates and x/yCoordinates are consistent`` (point: Point2D<Meters, TestSpace>) =
    let x, y = Point2D.coordinates point

    Test.all [
        Test.equal x point.X
        Test.equal y point.Y
        Test.equal x (Point2D.x point)
        Test.equal y (Point2D.y point)
    ]

[<Property>]
let ``coordinatesIn and x/yCoordinatesIn are consistent``
    (point: Point2D<Meters, TestSpace>)
    (frame: Frame2D<Meters, TestSpace, TestDefines>)
    =
    let x, y = Point2D.coordinatesIn frame point

    Test.all [
        Test.equal x (Point2D.xCoordinateIn frame point)
        Test.equal y (Point2D.yCoordinateIn frame point)
    ]

// ---- Modifiers ----

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

[<Property>]
let ``Rotation preserves distance``
    (point: Point2D<Meters, TestSpace>)
    (centerPoint: Point2D<Meters, TestSpace>)
    (rotationAngle: Angle)
    =

    let initialDistance = Point2D.distanceTo centerPoint point

    let rotatedPoint =
        Point2D.rotateAround centerPoint rotationAngle point

    let rotatedDistance =
        Point2D.distanceTo centerPoint rotatedPoint

    Test.equal initialDistance rotatedDistance

[<Property>]
let ``Project onto preserves distance`` (point: Point2D<Meters, TestSpace>) (axis: Axis2D<Meters, TestSpace>) =
    let initialDistance = Point2D.signedDistanceAlong axis point
    let projectedPoint = Point2D.projectOnto axis point

    let projectedDistance =
        Point2D.signedDistanceAlong axis projectedPoint

    Test.equal initialDistance projectedDistance

let ``translateBy and translateIn are consistent``
    (point: Point2D<Meters, TestSpace>)
    (direction: Direction2D<TestSpace>)
    (distance: Length<Meters>)
    =

    let displacement = Vector2D.withLength distance direction

    let translatedIn =
        Point2D.translateIn direction distance point

    let translatedBy = Point2D.translateBy displacement point

    Test.equal translatedIn translatedBy


// ---- Queries ----

[<Property>]
let ``Midpoint is equidistant`` (first: Point2D<Meters, TestSpace>) (second: Point2D<Meters, TestSpace>) =
    let midpoint = Point2D.midpoint first second
    Test.equal (Point2D.distanceTo midpoint first) (Point2D.distanceTo midpoint second)

[<Property>]
let ``Interpolation returns exact endpoints`` (first: Point2D<Meters, TestSpace>) (second: Point2D<Meters, TestSpace>) =
    Test.all [
        Test.equal first (Point2D.interpolateFrom first second 0.)
        Test.equal second (Point2D.interpolateFrom first second 1.)
    ]

[<Property>]
let ``Circumcenter of three points is equidistant from each point or is None``
    (p1: Point2D<Meters, TestSpace>)
    (p2: Point2D<Meters, TestSpace>)
    (p3: Point2D<Meters, TestSpace>)
    =
    match Point2D.circumcenter p1 p2 p3 with
    | None ->
        Triangle2D.area (Triangle2D.from p1 p2 p3)
        |> Test.equal Length.zero

    | Some circumcenter ->
        let r1 = Point2D.distanceTo circumcenter p1

        Test.all [
            Test.equal r1 (Point2D.distanceTo circumcenter p2)
            Test.equal r1 (Point2D.distanceTo circumcenter p3)
        ]

[<Test>]
let ``Tricky circumcenter case`` () =
    let p1 = Point2D.meters -10. 0.
    let p2 = Point2D.meters -10. 1.0e-6

    let p3 =
        Point2D.meters -9.858773586876941 4.859985890767644

    let expected =
        Point2D.meters 73.69327796224587 5.0e-7 |> Some

    let actual = Point2D.circumcenter p1 p2 p3

    Assert.AreEqual(expected, actual)


// ---- Conversion ----

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
