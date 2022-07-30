module GeometryTests.LineSegment2D

open FsCheck
open NUnit.Framework
open FsCheck.NUnit

open Geometry
open Units
open UnitsTests

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()

let distanceToTestSegment : LineSegment2D<Meters, TestSpace> =
    LineSegment2D.from (Point2D.meters 0. 5.) (Point2D.meters 5. 5.)

let ``Point distance test cases`` =
    let testCases : (string * Point2D<Meters, TestSpace> * Length) list =
        [ "Endpoint", (Point2D.meters 0. 5.), Length.meters 0.
          "Near start point", (Point2D.meters 5. 6.), Length.meters 1.
          "Near end point", (Point2D.meters 7. 5.), Length.meters 2.
          "Away from corner", (Point2D.meters -3. 1.), Length.meters 5.
          "Distance to segment", (Point2D.meters 3. 1.), Length.meters 4. ]

    testCases
    |> List.map
        (fun (name, point, expected) ->
            TestCaseData(point)
                .SetName(name)
                .Returns(expected))

[<TestCaseSource(nameof ``Point distance test cases``)>]
let ``Distance to point`` (point: Point2D<Meters, TestSpace>) =
    LineSegment2D.distanceToPoint point distanceToTestSegment

let ``Point closest to test cases`` =
    let testCases : (Point2D<Meters, TestSpace> * Point2D<Meters, TestSpace>) list =
        [ (Point2D.meters 0. 5.), (Point2D.meters 0. 5.)
          (Point2D.meters 5. 5.), (Point2D.meters 5. 5.)
          (Point2D.meters 2. 2.), (Point2D.meters 2. 5.)
          (Point2D.meters -3. 6.), (Point2D.meters 0. 5.) ]

    testCases
    |> List.map (fun (point, expected) -> TestCaseData(point).Returns(expected))

[<TestCaseSource(nameof ``Point closest to test cases``)>]
let ``Point closest to segment`` (point: Point2D<Meters, TestSpace>) =
    LineSegment2D.pointClosestTo point distanceToTestSegment

let pointOnLineTestCases =
    let testCases : Point2D<Meters, TestSpace> list =
        [ Point2D.meters 0. 5.
          Point2D.meters 5. 5.
          Point2D.meters 2.5 5.
          Point2D.meters 2.5 (5. + Float.Epsilon / 2.) ]

    testCases |> List.map TestCaseData

[<TestCaseSource(nameof pointOnLineTestCases)>]
let ``Point is on segment`` point =
    Assert.That(LineSegment2D.isPointOnSegment point distanceToTestSegment)

[<Test>]
let ``Line Segment Intersection`` () =
    let l1 =
        LineSegment2D.from (Point2D.meters 1. 1.) (Point2D.meters 4. 4.)

    let l2 =
        LineSegment2D.from (Point2D.meters 1. 4.) (Point2D.meters 4. 1.)

    let expected = Some(Point2D.meters 2.5 2.5)
    let actual = LineSegment2D.intersectionPoint l1 l2
    Assert.AreEqual(expected, actual)


// ---- Queries ----

[<Property>]
let ``Intersection lies on both line segments``
    (l1: LineSegment2D<Meters, TestSpace>)
    (l2: LineSegment2D<Meters, TestSpace>)
    =
    match LineSegment2D.intersectionPoint l1 l2 with
    | Some intersection ->
        LineSegment2D.isPointOnSegment intersection l1
        && LineSegment2D.isPointOnSegment intersection l2

    | None -> Test.pass

[<Property>]
let ``Intersection of two line segments returns a point that is on both segments if that point exists``
    (firstSegment: LineSegment2D<Meters, TestSpace>)
    (secondSegment: LineSegment2D<Meters, TestSpace>)
    =

    match LineSegment2D.intersectionPoint firstSegment secondSegment with

    | Some intersection ->
        // Not enough by itself - point might be collinear with
        // but not actually on the segment (e.g. past the end)
        let isCollinearWith (segment: LineSegment2D<Meters, TestSpace>) point =
            let area =
                Triangle2D.from segment.Start segment.Finish point
                |> Triangle2D.area

            Test.equal Quantity.zero area

        // Check that point is actually between the two
        // endpoints (almost enough of a check by itself, but
        // would not reliably detect small perpendicular
        // displacements from the segment)
        let isBetweenEndpointsOf (segment: LineSegment2D<Meters, TestSpace>) point =
            let firstDistance = Point2D.distanceTo segment.Start point
            let secondDistance = Point2D.distanceTo segment.Finish point
            Test.equal (LineSegment2D.length segment) (firstDistance + secondDistance)

        // Test if a point lies on a line segment
        let isOn (segment: LineSegment2D<Meters, TestSpace>) point =
            Test.all [
                isCollinearWith segment point
                isBetweenEndpointsOf segment point
            ]

        Test.all [
            isOn firstSegment intersection
            isOn secondSegment intersection
        ]


    | None ->
        match LineSegment2D.direction firstSegment, LineSegment2D.direction secondSegment with
        | Some firstDirection, Some secondDirection ->
            let firstAxis =
                Axis2D.through firstSegment.Start firstDirection

            let secondAxis =
                Axis2D.through secondSegment.Start secondDirection

            let onOneSideOf axis (segment: LineSegment2D<Meters, TestSpace>) =
                let startDistance =
                    Point2D.signedDistanceFrom axis segment.Start

                let endDistance =
                    Point2D.signedDistanceFrom axis segment.Finish

                let bothNonNegative =
                    startDistance > Length.meters -Float.Epsilon
                    && endDistance > Length.meters -Float.Epsilon

                let bothNonPositive =
                    startDistance < Length.meters Float.Epsilon
                    && endDistance < Length.meters Float.Epsilon

                bothNonNegative || bothNonPositive

            let firstBesideSecond = onOneSideOf secondAxis firstSegment

            let secondBesideFirst = onOneSideOf firstAxis secondSegment

            Test.isTrue "One segment is not fully on one side of the other" (firstBesideSecond || secondBesideFirst)

        | _ -> Test.pass


[<Property>]
let ``Intersection of two segments with a shared endpoint returns that endpoint``
    (firstStart: Point2D<Meters, TestSpace>)
    (secondStart: Point2D<Meters, TestSpace>)
    (sharedFinish: Point2D<Meters, TestSpace>)
    =
    let firstSegment =
        LineSegment2D.from firstStart sharedFinish

    let secondSegment =
        LineSegment2D.from secondStart sharedFinish

    let firstVector = LineSegment2D.vector firstSegment
    let secondVector = LineSegment2D.vector secondSegment

    let intersection =
        LineSegment2D.intersectionPoint firstSegment secondSegment

    if Vector2D.cross firstVector secondVector
       <> Quantity.zero then
        Test.equal (Some sharedFinish) intersection

    else
        Test.pass


[<Property>]
let ``Intersection of two collinear segments with one shared endpoint returns that endpoint``
    (startPoint: Point2D<Meters, TestSpace>)
    (vector: Vector2D<Meters, TestSpace>)
    =
    let tGen = Gen.floatBetween 0. 1. |> Arb.fromGen

    Prop.forAll
        tGen
        (fun t ->
            let endpoint = Point2D.translateBy vector startPoint

            let midpoint =
                Point2D.interpolateFrom startPoint endpoint t

            let firstSegment = LineSegment2D.from startPoint midpoint
            let secondSegment = LineSegment2D.from midpoint endpoint

            let intersection1 =
                LineSegment2D.intersectionPoint firstSegment secondSegment

            let intersection2 =
                LineSegment2D.intersectionPoint firstSegment (LineSegment2D.reverse secondSegment)

            let intersection3 =
                LineSegment2D.intersectionPoint (LineSegment2D.reverse firstSegment) secondSegment

            let intersection4 =
                LineSegment2D.intersectionPoint
                    (LineSegment2D.reverse firstSegment)
                    (LineSegment2D.reverse secondSegment)

            Test.all [
                Test.equal intersection1 (Some midpoint)
                Test.equal intersection2 (Some midpoint)
                Test.equal intersection3 (Some midpoint)
                Test.equal intersection4 (Some midpoint)
            ])


[<Property>]
let ``Intersection of trivial segment (a point) with itself is the point`` (point: Point2D<Meters, TestSpace>) =
    let segment = LineSegment2D.from point point

    let intersection =
        LineSegment2D.intersectionPoint segment segment

    Test.equal (Some point) intersection


[<Property>]
let ``Intersection of two identical non-degenerate line segments is nothing``
    (segment: LineSegment2D<Meters, TestSpace>)
    =

    if segment.Start = segment.Finish then
        Test.pass

    else
        LineSegment2D.intersectionPoint segment segment
        |> Test.equal None


[<Property>]
let ``Intersection of to reverse identical non-degenerate line segments is nothing``
    (segment: LineSegment2D<Meters, TestSpace>)
    =

    if segment.Start = segment.Finish then
        Test.pass

    else
        LineSegment2D.reverse segment
        |> LineSegment2D.intersectionPoint segment
        |> Test.equal None

[<Property>]
let ``A shared endpoint on a third segment induces an intersection between the third segment and the last one of the other two segments sharing that endpoint``
    (segment3: LineSegment2D<Meters, TestSpace>)
    (point1: Point2D<Meters, TestSpace>)
    (point2: Point2D<Meters, TestSpace>)
    =

    let sharedPoint = LineSegment2D.midpoint segment3
    let segment1 = LineSegment2D.from sharedPoint point1
    let segment2 = LineSegment2D.from sharedPoint point2
    let v1 = LineSegment2D.vector segment1
    let v2 = LineSegment2D.vector segment2
    let v3 = LineSegment2D.vector segment3

    let intersections =
        (LineSegment2D.intersectionPoint segment1 segment3), (LineSegment2D.intersectionPoint segment2 segment3)

    let v3Xv1 = Vector2D.cross v1 v3
    let v3Xv2 = Vector2D.cross v2 v3

    if v3Xv1 = Quantity.zero || v3Xv2 = Quantity.zero then
        Test.pass

    else if v3Xv1 * v3Xv2 > Quantity.zero then
        match intersections with
        | None, None -> Test.pass

        // If intersection points are found for both
        // segments, they should be both be approximately
        // equal to the shared endpoint
        | Some p1, Some p2 ->
            Test.all [
                Test.equal p1 sharedPoint
                Test.equal p2 sharedPoint
            ]

        // If an intersection point is found for only segment1,
        // then that point should be approximately equal to
        // sharedPoint and segment2 should be approximately
        // parallel to segment3
        | Some p1, None ->
            Test.all [
                Test.equal p1 sharedPoint
                Test.equal v3Xv2 Quantity.zero
            ]

        // If an intersection point is found for only segment2,
        // then that point should be approximately equal to
        // sharedPoint and segment1 should be approximately
        // parallel to segment3
        | None, Some p2 ->
            Test.all [
                Test.equal p2 sharedPoint
                Test.equal v3Xv1 Quantity.zero
            ]

    else
        // point1 and point2 are on opposite sides of segment3
        match intersections with
        | None, None -> Test.fail "Shared endpoint intersection not found"

        // If an intersection point is found for one
        // segment, it should be approximately equal to
        // the shared endpoint
        | Some point, None -> Test.equal sharedPoint point

        // If an intersection point is found for one
        // segment, it should be approximately equal to
        // the shared endpoint
        | None, Some point -> Test.equal sharedPoint point

        // If intersection points are found for both
        // segments, they should be both be approximately
        // equal to the shared endpoint
        | Some p1, Some p2 ->

            Test.all [
                Test.equal sharedPoint p1
                Test.equal sharedPoint p2
            ]

[<Property>]
let ``Intersection should be approximately symmetric``
    (lineSegment1: LineSegment2D<Meters, TestSpace>)
    (lineSegment2: LineSegment2D<Meters, TestSpace>)
    =
    let intersection12 =
        LineSegment2D.intersectionPoint lineSegment1 lineSegment2

    let intersection21 =
        LineSegment2D.intersectionPoint lineSegment2 lineSegment1

    match intersection12, intersection21 with
    | Some p1, Some p2 -> Test.equal p1 p2
    | _ -> Test.equal intersection12 intersection21


[<Property>]
let ``Reversing one line segment should not change the intersection point``
    (lineSegment1: LineSegment2D<Meters, TestSpace>)
    (lineSegment2: LineSegment2D<Meters, TestSpace>)
    =
    let normalIntersection =
        LineSegment2D.intersectionPoint lineSegment1 lineSegment2

    let reversedIntersection =
        LineSegment2D.intersectionPoint lineSegment1 (LineSegment2D.reverse lineSegment2)

    match normalIntersection, reversedIntersection with
    | Some p1, Some p2 -> Test.equal p1 p2

    | _ -> Test.equal normalIntersection reversedIntersection


[<Property>]
let ``signedDistanceFrom contains distance for any point on the line segment``
    (lineSegment: LineSegment2D<Meters, TestSpace>)
    (axis: Axis2D<Meters, TestSpace>)
    =
    let tGen = Gen.floatBetween 0. 1. |> Arb.fromGen

    Prop.forAll
        tGen
        (fun t ->
            let point = LineSegment2D.interpolate lineSegment t

            LineSegment2D.signedDistanceFrom axis lineSegment
            |> Interval.contains (Point2D.signedDistanceFrom axis point)
            |> Test.isTrue "Interval should contain distance for any point on the line segment")


[<Property>]
let ``signedDistanceAlong contains distance for any point on the line segment``
    (lineSegment: LineSegment2D<Meters, TestSpace>)
    (axis: Axis2D<Meters, TestSpace>)
    =
    let tGen = Gen.floatBetween 0. 1. |> Arb.fromGen

    Prop.forAll
        tGen
        (fun t ->
            let point = LineSegment2D.interpolate lineSegment t

            let signedDistance =
                LineSegment2D.signedDistanceAlong axis lineSegment

            let signedDistanceAlongAxis = Point2D.signedDistanceAlong axis point

            Interval.contains signedDistanceAlongAxis signedDistance
            |> Test.isTrue "Interval should contain distance for any point on the line segment")
