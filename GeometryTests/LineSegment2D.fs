module GeometryTests.LineSegment2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry
open FSharp.Extensions

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()

let ``Point distance test cases`` =
    [ "Endpoint", (Point2D.meters 0. 5.), Length.meters 0.
      "Near start point", (Point2D.meters 5. 6.), Length.meters 1.
      "Near end point", (Point2D.meters 7. 5.), Length.meters 2.
      "Away from corner", (Point2D.meters -3. 1.), Length.meters 5.
      "Distance to segment", (Point2D.meters 3. 1.), Length.meters 4. ]
    |> List.map
        (fun (name, point, expected) ->
            TestCaseData(point)
                .SetName(name)
                .Returns(expected))

[<TestCaseSource(nameof ``Point distance test cases``)>]
let ``Distance to point`` (point: Point2D<Meters, 'Coordinates>) =
    let line =
        LineSegment2D.from (Point2D.meters 0. 5.) (Point2D.meters 5. 5.)

    LineSegment2D.distanceToPoint point line

let ``Point closest to test cases`` =
    let line =
        LineSegment2D.from (Point2D.meters 0. 5.) (Point2D.meters 5. 5.)

    [ (Point2D.meters 0. 5.), line, (Point2D.meters 0. 5.)
      (Point2D.meters 5. 5.), line, (Point2D.meters 5. 5.)
      (Point2D.meters 2. 2.), line, (Point2D.meters 2. 5.)
      (Point2D.meters -3. 6.), line, (Point2D.meters 0. 5.) ]
    |> List.map (fun (point, line, expected) -> TestCaseData(point, line).Returns(expected))

[<TestCaseSource(nameof ``Point closest to test cases``)>]
let ``Point closest to line`` (point: Point2D<Meters, 'Coordinates>) (line: LineSegment2D<Meters, 'Coordinates>) =
    LineSegment2D.pointClosestTo point line

let pointOnLineTestCases =
    let line =
        LineSegment2D.from (Point2D.meters 0. 5.) (Point2D.meters 5. 5.)

    [ (Point2D.meters 0. 5.), line
      (Point2D.meters 5. 5.), line
      (Point2D.meters 2.5 5.), line
      (Point2D.meters 2.5 (5. + Epsilon / 2.), line) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Point is on line`` point line =
    Assert.That(LineSegment2D.isPointOnLine point line)

[<Test>]
let ``Line Segment Intersection`` () =
    let l1 =
        LineSegment2D.from (Point2D.meters 1. 1.) (Point2D.meters 4. 4.)
        
    let l2 =
        LineSegment2D.from (Point2D.meters 1. 4.) (Point2D.meters 4. 1.)

    let expected = Some(Point2D.meters 2.5 2.5)
    let actual = LineSegment2D.intersectionPoint l1 l2
    Assert.AreEqual(expected, actual)

[<Property>]
let ``Intersection lies on both line segments``
    (l1: LineSegment2D<Meters, TestSpace>)
    (l2: LineSegment2D<Meters, TestSpace>)
    =
    match LineSegment2D.intersectionPoint l1 l2 with
    | Some intersection ->
        LineSegment2D.isPointOnLine intersection l1
        && LineSegment2D.isPointOnLine intersection l2
    | None -> true
