module GeometryTests.Circle2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Units
open UnitsTests


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

let ``Point inside circle test cases`` =
    [ "Point is center point", Point2D.meters 1. 1., true
      "Point is inside", Point2D.meters 2. 2., true
      "Point is on the edge", Point2D.meters 1. 4., true
      "Point is outside", Point2D.meters 5. 5., false ]
    |> List.map
        (fun (name, point, expected) ->
            TestCaseData(point)
                .SetName(name)
                .Returns(expected))


[<TestCaseSource(nameof ``Point inside circle test cases``)>]
let ``Circle contains point`` (point: Point2D<Meters, 'Coordinates>) =
    let circle : Circle2D<Meters, 'Coordinates> =
        Circle2D.atPoint (Point2D.meters 1. 1.) (Length.meters 3.)

    Circle2D.containsPoint point circle

[<Test>]
let ``Get bounding box`` () =
    let actual =
        Circle2D.atOrigin (Length.meters 5.)
        |> Circle2D.boundingBox

    let expected =
        BoundingBox2D.fromExtrema
            { MinX = Length.meters -5.0
              MaxX = Length.meters 5.0
              MinY = Length.meters -5.0
              MaxY = Length.meters 5.0 }

    Assert.AreEqual(expected, actual)

[<Property>]
let ``A circle's bounding box contains it's center point`` (circle: Circle2D<Meters, TestSpace>) =
    let boundingBox = Circle2D.boundingBox circle
    let centerPoint = Circle2D.centerPoint circle

    Test.isTrue
        "Circle's bounding box does not contains the center point"
        (BoundingBox2D.contains centerPoint boundingBox)



module ``Intersection between a circle and a bounding box`` =
    let someBox x1 x2 y1 y2 =
        { MinX = Length.meters x1
          MaxX = Length.meters x2
          MinY = Length.meters y1
          MaxY = Length.meters y2 }

    let someCircle r center =
        Circle2D.withRadius (Length.meters r) center

    let noIntersectionFound = "Expected an intersection to be found"
    let unexpectedIntersection = "Expected no intersection to be found"

    [<Test>]
    let ``Detects intersection when overlapping in both X and Y`` () =
        let box = someBox 1. 5. 1. 5.
        let circle = someCircle 2. Point2D.origin

        Assert.IsTrue(Circle2D.intersectsBoundingBox box circle, noIntersectionFound)


    [<Test>]
    let ``Detects no intersection when not overlapping`` () =
        let box = someBox 20. 22. 30. 40.
        let circle = someCircle 5. (Point2D.meters -20. -20.)

        Assert.IsFalse(Circle2D.intersectsBoundingBox box circle, unexpectedIntersection)


    [<Test>]
    let ``Detects intersects when box and circle touch by exactly one pixel`` () =
        let box = someBox 1. 1. 0. 0.
        let circle = someCircle 1. Point2D.origin

        Assert.IsTrue(Circle2D.intersectsBoundingBox box circle, noIntersectionFound)
