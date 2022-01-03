module GeometryTests.Line2D

open NUnit.Framework
open FsCheck.NUnit


open Geometry
open Utilities.Extensions

[<SetUp>]
let Setup () =
    Gen.ArbGeometry.Register()

let pointClosestToTestCases =
    let line =
        Line2D.through (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line, (Point2D.xy 0. 5.)
      (Point2D.xy 5. 5.), line, (Point2D.xy 5. 5.)
      (Point2D.xy 2. 2.), line, (Point2D.xy 2. 5.) ]
    |> List.map (fun (point, line, expected) -> TestCaseData(point, line).Returns(expected))

[<TestCaseSource(nameof pointClosestToTestCases)>]
let ``Point closest to line`` vertex line = Line2D.pointClosestTo vertex line

let pointOnLineTestCases =
    let line =
        Line2D.through (Point2D.xy 0. 5.) (Point2D.xy 5. 5.)

    [ (Point2D.xy 0. 5.), line
      (Point2D.xy 5. 5.), line
      (Point2D.xy 2.5 5.), line
      (Point2D.xy 2.5 (5. + Epsilon / 2.), line) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Vertex is on line`` vertex line =
    Assert.That(Line2D.isPointOnLine vertex line)

[<Property>]
let ``Line perpendicular through point`` point line =
    let perpLine = Line2D.perpThroughPoint point line

    Line2D.arePerpendicular perpLine line
    && Line2D.isPointOnLine point perpLine

[<Test>]
let ``Line Intersection`` () =
    let l1 =
        Line2D.through (Point2D.xy 1. 4.) (Point2D.xy 4. 1.)

    let l2 =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = Some(Point2D.xy 2.5 2.5)
    let actual = Line2D.intersect l1 l2
    Assert.AreEqual(expected, actual)

[<Property>]
let ``Intersection lies on both lines`` l1 l2 =
    match Line2D.intersect l1 l2 with
    | Some intersection ->
        Line2D.isPointOnLine intersection l1
        && Line2D.isPointOnLine intersection l2
    | None -> true
