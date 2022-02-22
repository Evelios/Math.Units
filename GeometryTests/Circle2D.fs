module GeometryTests.Circle2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry
open FSharp.Extensions


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
    let circle: Circle2D<Meters, 'Coordinates> =
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
