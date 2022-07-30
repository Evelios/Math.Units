module GeometryTests.Polygon2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Units
open UnitsTests

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Property>]
[<Ignore("I need to figure this out")>]
let ``The convex hull of a list of points is actually convex`` (points: Point2D<Meters, TestSpace> list) =
    let convexHull = Polygon2D.convexHull points

    let edgeVectors =
        Polygon2D.edges convexHull
        |> List.map LineSegment2D.vector

    match edgeVectors with
    | [] -> Test.pass
    | [ _ ] -> Test.fail "Polygon should never have a single edge"
    | first :: rest ->
        let crossProducts =
            List.map2 (fun v1 v2 -> v1 |> Vector2D.cross v2) (first :: rest) (rest @ [ first ])

        Test.isTrue "Edges should always turn counterclockwise" (List.forall ((>=) Quantity.zero) crossProducts)


[<Property>]
let ``The convex hull of a list of points contains all of those points`` (points: Point2D<Meters, TestSpace> list) =
    let convexHull = Polygon2D.convexHull points
    let edges = Polygon2D.edges convexHull

    let isNonNegativeArea point edge =
        let p1, p2 = LineSegment2D.endpoints edge
        let triangle = Triangle2D.from point p1 p2

        Triangle2D.counterclockwiseArea triangle
        >= Quantity.zero

    let isContained point =
        List.forall (isNonNegativeArea point) edges

    Test.isTrue "Convex hull should contain all points" (List.forall isContained points)


let simplePolygon : Polygon2D<Meters, TestSpace> =
    Polygon2D.singleLoop [
        Point2D.meters 1. 1.
        Point2D.meters 3. 1.
        Point2D.meters 3. 2.
        Point2D.meters 1. 2.
    ]


let withHole : Polygon2D<Meters, TestSpace> =
    Polygon2D.withHoles [ [ Point2D.meters 1. 1.
                            Point2D.meters 1. 2.
                            Point2D.meters 2. 2.
                            Point2D.meters 2. 1. ] ] [
        Point2D.meters 0. 0.
        Point2D.meters 3. 0.
        Point2D.meters 3. 3.
        Point2D.meters 0. 3.
    ]


let ``Polygon contains point test cases`` =
    let testCases : (string * Polygon2D<Meters, TestSpace> * Point2D<Meters, TestSpace> * bool) list =
        [ "Inside", simplePolygon, Point2D.meters 2. 1.5, true
          "Boundary", simplePolygon, Point2D.meters 2. 1.5, true
          "Outside", simplePolygon, Point2D.meters 4. 1.5, false
          "Inside with hole", withHole, Point2D.meters 2. 2.5, true
          "Boundary of hole", withHole, Point2D.meters 2. 2., true
          "Outside (in the hole)", withHole, Point2D.meters 1.5 15., false ]

    testCases
    |> List.map
        (fun (name, poly, point, result) ->
            TestCaseData(poly, point)
                .SetName(name)
                .Returns(result))

[<TestCaseSource(nameof ``Polygon contains point test cases``)>]
let ``Polygon contains point`` (polygon: Polygon2D<Meters, TestSpace>) (point: Point2D<Meters, TestSpace>) =
    Polygon2D.contains point polygon


// Todo: Need a robust polygon generator
// https://github.com/ianmackenzie/elm-geometry/blob/fdb7f1056625f09de930237177f52ced7066bb0a/tests/Polygon2d/Random.elm
[<Property>]
[<Ignore("I need to figure this out")>]
let ``Rotating a polygon around its centroid keeps the centroid point``
    (polygon: Polygon2D<Meters, TestSpace>)
    (angle: Angle)
    =
    match Polygon2D.centroid polygon with
    | None -> Test.fail "Original polygon needs a centroid"
    | Some centroid ->

        match polygon
              |> Polygon2D.rotateAround centroid angle
              |> Polygon2D.centroid with

        | Some rotatedCentroid -> centroid .=. rotatedCentroid
        | None -> Test.fail "Rotated polygon needs a centroid"
