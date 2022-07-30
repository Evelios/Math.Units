module GeometryTests.Axis2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Units
open UnitsTests


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Test>]
let ``X Builder`` () =
    let xAxis = Axis2D.x

    let expected =
        Axis2D.through Point2D.origin Direction2D.x

    Assert.AreEqual(expected, xAxis)


[<Test>]
let ``Y Builder`` () =
    let xAxis = Axis2D.y

    let expected =
        Axis2D.through Point2D.origin Direction2D.y

    Assert.AreEqual(expected, xAxis)


[<Test>]
let Reverse () =
    let actual = Axis2D.reverse Axis2D.x

    let expected =
        Axis2D.through Point2D.origin Direction2D.negativeX

    Assert.AreEqual(expected, actual)


[<Test>]
let ``Move To`` () =
    let axis =
        Axis2D.withDirection Direction2D.y (Point2D.meters 4. 5.)

    let newOrigin = Point2D.meters 4. 5.
    let actual = Axis2D.moveTo newOrigin axis

    let expected =
        Axis2D.withDirection Direction2D.y (Point2D.meters 4. 5.)

    Assert.AreEqual(expected, actual)


[<Test>]
let ``Rotate around`` () =
    let actual =
        Axis2D.rotateAround Point2D.origin Angle.halfPi Axis2D.x

    let expected = Axis2D.y

    Assert.AreEqual(expected, actual)


[<Test>]
let ``rotateAround example`` () =
    let rotated =
        Axis2D.rotateAround Point2D.origin (Angle.degrees 90.) Axis2D.x

    Assert.AreEqual(Axis2D.y, rotated)

[<Test>]
let ``translateBy example`` () =
    let displacement = Vector2D.meters 2. 3.

    let expected =
        Axis2D.withDirection Direction2D.y (Point2D.meters 2. 3.)

    let actual = Axis2D.translateBy displacement Axis2D.y

    Assert.AreEqual(expected, actual)

[<Test>]
let ``mirrorAcross example`` () =
    let axis =
        Axis2D.through (Point2D.meters 1. 2.) (Direction2D.fromAngle (Angle.degrees 30.))

    let expected =
        Axis2D.through (Point2D.meters 1. -2.) (Direction2D.fromAngle (Angle.degrees -30.))

    let actual = Axis2D.mirrorAcross Axis2D.x axis
    Assert.AreEqual(expected, actual)


[<Test>]
let ``relativeTo example`` () =
    let origin = Point2D.meters 2. 3.

    let expected =
        Axis2D.withDirection Direction2D.x (Point2D.meters 2. 3.)

    let actual =
        Axis2D.placeIn (Frame2D.atPoint origin) Axis2D.x

    Assert.AreEqual(expected, actual)

[<Test>]
let ``placeInExample example`` () =
    let origin = Point2D.meters 2. 3.

    let expected =
        Axis2D.withDirection Direction2D.x (Point2D.meters 2. 3.)

    let actual =
        Axis2D.placeIn (Frame2D.atPoint origin) Axis2D.x

    Assert.AreEqual(expected, actual)

[<Property>]
let throughPoints (p1: Point2D<Meters, TestSpace>) (p2: Point2D<Meters, TestSpace>) =
    match Axis2D.throughPoints p1 p2 with
    | Some axis ->
        Test.all [
            Test.equal p1 axis.Origin
            Test.equal Quantity.zero (Point2D.signedDistanceFrom axis p2)
        ]
    | None -> Test.equal p1 p2
