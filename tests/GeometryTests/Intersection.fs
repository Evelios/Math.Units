module GeometryTests.Intersection

open FsCheck
open NUnit.Framework
open Geometry

[<SetUp>]
let Setup () = ()

[<Test>]
let ``Line Segment And Line Intersection`` () =
    let segment =
        LineSegment2D.from (Point2D.xy 1. 4.) (Point2D.xy 4. 1.)

    let line =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = Some(Point2D.xy 2.5 2.5)

    let actual =
        Intersection2D.lineSegmentAndLine segment line

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Line Segment And Line No Intersection`` () =
    let segment =
        LineSegment2D.from (Point2D.xy 1. 4.) (Point2D.xy 2. 3.)

    let line =
        Line2D.through (Point2D.xy 1. 1.) (Point2D.xy 4. 4.)

    let expected = None

    let actual =
        Intersection2D.lineSegmentAndLine segment line

    Assert.AreEqual(expected, actual)
