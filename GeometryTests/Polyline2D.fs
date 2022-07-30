module GeometryTests.Polyline2D

open FsCheck.NUnit
open FsCheck

open Geometry
open Units
open UnitsTests

[<Property>]
let ``Centriod is None if polyline is empty`` =
    let emptyPolyline = Polyline2D.fromVertices []

    Polyline2D.centroid emptyPolyline
    |> Test.equal None


[<Property>]
let ``Centroid of zero length polyline is the same point`` (point: Point2D<Meters, TestSpace>) =
    let tGen = Gen.intBetween 1 20 |> Arb.fromGen

    Prop.forAll
        tGen
        (fun reps ->
            let singlePointLine =
                List.replicate reps point
                |> Polyline2D.fromVertices

            Polyline2D.centroid singlePointLine
            |> Test.equal (Some point))


[<Property>]
let ``Centroid of single line segment is middle of endpoints``
    (p1: Point2D<Meters, TestSpace>)
    (p2: Point2D<Meters, TestSpace>)
    =
    let singleLine = Polyline2D.fromVertices [ p1; p2 ]
    let expectedCentroid = Point2D.midpoint p1 p2

    Polyline2D.centroid singleLine
    |> Test.equal (Some expectedCentroid)


[<Property>]
let ``Centroid of a right angle is between the two sides`` (armLength: Length) =
    let angle =
        Polyline2D.fromVertices [
            Point2D.xy Length.zero Length.zero
            Point2D.xy armLength Length.zero
            Point2D.xy armLength armLength
        ]

    let expectedCentroid =
        Point2D.xy (0.75 * armLength) (0.25 * armLength)

    Polyline2D.centroid angle
    |> Test.equal (Some expectedCentroid)


[<Property>]
let ``Centroid of a step shape is halfway up the step`` (armLength: Length) =

    let angle =
        Polyline2D.fromVertices [
            Point2D.xy Length.zero Length.zero
            Point2D.xy armLength Length.zero
            Point2D.xy armLength armLength
            Point2D.xy (2. * armLength) armLength
        ]

    let expectedCentroid = Point2D.xy armLength (armLength / 2.)

    Polyline2D.centroid angle
    |> Test.equal (Some expectedCentroid)


[<Property>]
let ``Centroid of an open square is skewed to closed side`` (sideLength: Length) =
    let squareLine =
        Polyline2D.fromVertices [
            Point2D.xy Length.zero Length.zero
            Point2D.xy Length.zero sideLength
            Point2D.xy sideLength sideLength
            Point2D.xy sideLength Length.zero
        ]

    let expectedCentroid =
        Point2D.xy (sideLength / 2.) (sideLength * 2. / 3.)

    Polyline2D.centroid squareLine
    |> Test.equal (Some expectedCentroid)


[<Property>]
let ``Centroid of a closed square is mid-point`` (sideLength: Length) =
    let squareLine =
        Polyline2D.fromVertices [
            Point2D.xy Length.zero Length.zero
            Point2D.xy Length.zero sideLength
            Point2D.xy sideLength sideLength
            Point2D.xy sideLength Length.zero
            Point2D.xy Length.zero Length.zero
        ]

    let expectedCentroid =
        Point2D.xy (sideLength / 2.) (sideLength / 2.)

    Polyline2D.centroid squareLine
    |> Test.equal (Some expectedCentroid)


[<Property>]
let ``The centroid of a polyline is within the polyline's bounding box``
    (first: Point2D<Meters, TestSpace>)
    (second: Point2D<Meters, TestSpace>)
    (rest: Point2D<Meters, TestSpace> list)
    =
    let points = first :: second :: rest
    let polyline = Polyline2D.fromVertices points
    let maybeBoundingBox = Polyline2D.boundingBox polyline
    let maybeCentroid = Polyline2D.centroid polyline

    match maybeBoundingBox, maybeCentroid with
    | Some boundingBox, Some centroid ->
        Test.isTrue "Centroid is not contained in bounding box" (BoundingBox2D.contains centroid boundingBox)
    | None, _ -> Test.fail "Error determining bounding box"
    | _, None -> Test.fail "Error determining centroid"
