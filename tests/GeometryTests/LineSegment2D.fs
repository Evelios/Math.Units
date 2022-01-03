module GeometryTests.LineSegment2D

open NUnit.Framework
open FsCheck.NUnit

open Utilities.Extensions
open Geometry

[<SetUp>]
let SetUp () =
    Gen.ArbGeometry.Register()

let pointDistanceTestCases =
    [ "Endpoint", (Point2D.xy 0. 5.), 0.
      "Near start point", (Point2D.xy 5. 6.), 1.
      "Near end point", (Point2D.xy 7. 5.), 2.
      "Away from corner", (Point2D.xy -3. 1.), 5.
      "Distance to segment", (Point2D.xy 3. 1.), 4. ]
    |> List.map
        (fun (name, point, expected) ->
            TestCaseData(point)
                .SetName(name)
                .Returns(expected))

[<TestCaseSource(nameof pointDistanceTestCases)>]
let ``Distance to point`` point =
    let line =
        LineSegment2D.from (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    LineSegment2D.distanceToPoint point line

let pointClosestToTestCases =
    let line =
        LineSegment2D.from (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line, (Point2D.xy 0. 5.)
      (Point2D.xy 5. 5.), line, (Point2D.xy 5. 5.)
      (Point2D.xy 2. 2.), line, (Point2D.xy 2. 5.)
      (Point2D.xy -3. 6.), line, (Point2D.xy 0. 5.) ]
    |> List.map (fun (point, line, expected) -> TestCaseData(point, line).Returns(expected))

[<TestCaseSource(nameof pointClosestToTestCases)>]
let ``Point closest to line`` point line = LineSegment2D.pointClosestTo point line

let pointOnLineTestCases =
    let line =
        LineSegment2D.from (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line
      (Point2D.xy 5. 5.), line
      (Point2D.xy 2.5 5.), line
      (Point2D.xy 2.5 (5. + Epsilon / 2.), line) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Point is on line`` point line =
    Assert.That(LineSegment2D.isPointOnLine point line)

[<Test>]
let ``Line Segment Intersection`` () =
    let l1 =
        LineSegment2D.from (Point2D.xy 1. 4.) (Point2D.xy 4. 1.)

    let l2 =
        LineSegment2D.from (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = Some(Point2D.xy 2.5 2.5)
    let actual = LineSegment2D.intersect l1 l2
    Assert.AreEqual(expected, actual)

[<Property>]
let ``Intersection lies on both line segments`` l1 l2 =
    match LineSegment2D.intersect l1 l2 with
    | Some intersection ->
        LineSegment2D.isPointOnLine intersection l1
        && LineSegment2D.isPointOnLine intersection l2
    | None -> true
