module GeometryTests.LineSegment2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry
open FSharp.Extensions

[<SetUp>]
let SetUp () = Gen.ArbGeometry.Register()

let distanceToTestSegment: LineSegment2D<Meters, TestSpace> =
    LineSegment2D.from (Point2D.meters 0. 5.) (Point2D.meters 5. 5.)

let ``Point distance test cases`` =
    let testCases: (string * Point2D<Meters, TestSpace> * Length<Meters>) list =
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
    let testCases: (Point2D<Meters, TestSpace> * Point2D<Meters, TestSpace>) list =
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
          Point2D.meters 2.5 (5. + Epsilon / 2.) ]
        
    testCases
    |> List.map TestCaseData

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
    | None -> true

[<Property>]
let ``Intersection of two line segments returns a point that is on both segments if that point exists``
    (firstSegment: LineSegment2D<Meters, TestSpace>)
    (secondSegment: LineSegment2D<Meters, TestSpace>)
    =

    match LineSegment2D.intersectionPoint firstSegment secondSegment with

    | Some intersection ->
        // Not enough by itself - point might be collinear with
        // but not actually on the segment (e.g. past the end)
        let isCollinearWith (segment: LineSegment2D<'Unit, 'Coordinates>) point =
            let area =
                Triangle2D.from segment.Start segment.Finish point
                |> Triangle2D.area

            Test.equal Length.zero area

        // Check that point is actually between the two
        // endpoints (almost enough of a check by itself, but
        // would not reliably detect small perpendicular
        // displacements from the segment)
        let isBetweenEndpointsOf (segment: LineSegment2D<'Unit, 'Coordinates>) point =
            let firstDistance = Point2D.distanceTo segment.Start point
            let secondDistance = Point2D.distanceTo segment.Finish point
            Test.equal (LineSegment2D.length segment) (firstDistance + secondDistance)

        // Test if a point lies on a line segment
        let isOn (segment: LineSegment2D<'Unit, 'Coordinates>) point =
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

        | _ -> true
