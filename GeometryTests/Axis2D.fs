module GeometryTests.Axis2D

open NUnit.Framework
open FsCheck

open Geometry


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<TestCase>]
let ``X Builder`` () =
    let xAxis = Axis2D.x

    let expected =
        Axis2D.through Point2D.origin Direction2D.x

    Assert.AreEqual(expected, xAxis)


[<TestCase>]
let ``Y Builder`` () =
    let xAxis = Axis2D.y

    let expected =
        Axis2D.through Point2D.origin Direction2D.y

    Assert.AreEqual(expected, xAxis)


[<TestCase>]
let Reverse () =
    let actual = Axis2D.reverse Axis2D.x

    let expected =
        Axis2D.through Point2D.origin Direction2D.negativeX

    Assert.AreEqual(expected, actual)


[<TestCase>]
let ``Move To`` () =
    let axis =
        Axis2D.withDirection Direction2D.y (Point2D.meters 4. 5.)

    let newOrigin = Point2D.meters 4. 5.
    let actual = Axis2D.moveTo newOrigin axis

    let expected =
        Axis2D.withDirection Direction2D.y (Point2D.meters 4. 5.)

    Assert.AreEqual(expected, actual)


[<TestCase>]
let ``Rotate around`` () =
    let actual =
        Axis2D.rotateAround Point2D.origin Angle.halfPi Axis2D.x

    let expected = Axis2D.y

    Assert.AreEqual(expected, actual)
