module GeometryTests.BoundingBox2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry
open Units
open UnitsTests


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Property>]
let ``intersection is consistent with intersects``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =
    let intersects =
        BoundingBox2D.intersects first second

    let intersection =
        BoundingBox2D.intersection first second

    match intersects, intersection with
    | true, Some _ -> Test.pass
    | false, None -> Test.pass
    | true, None -> Test.fail $"{first} and {second} are considered to intersect, but intersection is None"
    | false, Some intersectionBox ->
        Test.fail $"{first} and {second} are not considered to intersect, but intersection is {intersectionBox}"


[<Ignore("Need to fix later")>]
[<Property>]
let ``intersection is consistent with overlappingByAtLeast``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =

    let overlapping =
        BoundingBox2D.overlappingByAtLeast Length.zero first second

    let intersection =
        BoundingBox2D.intersection first second

    match overlapping, intersection with
    | true, Some _ -> Test.pass
    | false, None -> Test.pass
    | true, None -> Test.fail $"{first} and {second} are considered to intersect/overlap, but intersection is None"
    | false, Some intersectionBox ->
        Test.fail $"{first} and {second} are not considered to intersect/overlap, but intersection is {intersectionBox}"


[<Property>]
let ``Bounding box union contains both boxes``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =
    let union = BoundingBox2D.union first second

    BoundingBox2D.isContainedIn first union
    && BoundingBox2D.isContainedIn second union


[<Ignore("Need to fix later")>]
[<Property>]
let ``Intersection of two bounding boxes is either Nothing or just a valid box``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =

    match BoundingBox2D.intersection first second with
    | None -> Test.pass
    | Some result -> Test.isValidBoundingBox2D result


[<Ignore("Need to fix later")>]
[<Property>]
let ``Box contains own center point`` (box: BoundingBox2D<Meters, TestSpace>) =
    BoundingBox2D.contains (BoundingBox2D.centerPoint box) box


[<Ignore("Need to fix later")>]
[<Property>]
let ``overlappingByAtLeast detects non-intersecting boxes``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =

    match BoundingBox2D.intersection first second with
    | Some _ ->
        Test.isTrue
            "intersecting boxes should overlap by at least 0"
            (BoundingBox2D.overlappingByAtLeast Length.zero first second)

    | None ->
        Test.isFalse
            "non-intersecting boxes should overlap by less than 0"
            (BoundingBox2D.overlappingByAtLeast Length.zero first second)


[<Property>]
let ``Boxes overlapping greater than a distance cannot be separated by moving that distance``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    (displacement: Vector2D<Meters, TestSpace>)
    =
    let tolerance = Vector2D.length displacement

    if BoundingBox2D.overlappingByAtLeast tolerance first second then
        BoundingBox2D.translateBy displacement first
        |> BoundingBox2D.intersects second
        |> Test.isTrue "Displaced box should still intersect with the other box"

    else
        Test.pass

[<Ignore("Need to fix this later")>]
[<Property>]
let ``Boxes separated by greater than a distance cannot be made to overlap by moving that distance``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    (displacement: Vector2D<Meters, TestSpace>)
    =
    let tolerance = Vector2D.length displacement

    if BoundingBox2D.separatedByAtLeast tolerance first second then
        BoundingBox2D.translateBy displacement first
        |> BoundingBox2D.intersects second
        |> Test.isFalse "Displaced box should still intersect with the other box"

    else
        Test.pass


let ``Separation test cases`` =
    [ "Expected separation to be greater than 0.5", Length.unitless 0.5, true
      "Expected separation to be greater than 0", Length.unitless 0., true
      "Expected separation to be greater than -1", Length.unitless -1., true
      "Expected separation to be greater than 0.99", Length.unitless 0.99, true
      "Expected separation to not be greater than 1.01", Length.unitless 1.01, false ]
    |> List.map (fun (name, greaterThan, expected) ->
        TestCaseData(greaterThan)
            .SetName(name)
            .Returns(expected))

[<TestCaseSource(nameof ``Separation test cases``)>]
let ``separation is determined correctly for horizontally displaced boxes`` (separation: Quantity<Unitless>) =
    let firstBox: BoundingBox2D<Unitless, TestSpace> =
        { MinX = Length.unitless 0.
          MinY = Length.unitless 0.
          MaxX = Length.unitless 1.
          MaxY = Length.unitless 1. }

    let secondBox: BoundingBox2D<Unitless, TestSpace> =
        { MinX = Length.unitless 2.
          MinY = Length.unitless 0.
          MaxX = Length.unitless 3.
          MaxY = Length.unitless 1. }

    BoundingBox2D.separatedByAtLeast separation firstBox secondBox


[<TestCaseSource(nameof ``Separation test cases``)>]
let ``separation is determined correctly for vertically displaced boxes`` (separation: Quantity<Unitless>) =
    let firstBox =
        { MinX = Length.unitless 0.
          MinY = Length.unitless 0.
          MaxX = Length.unitless 1.
          MaxY = Length.unitless 1. }

    let secondBox =
        { MinX = Length.unitless 0.
          MinY = Length.unitless 2.
          MaxX = Length.unitless 1.
          MaxY = Length.unitless 3. }

    BoundingBox2D.separatedByAtLeast separation firstBox secondBox


let ``Diagonal separation test cases`` =
    [ "Expected separation to be greater than 0.5", Length.unitless 4., true
      "Expected separation to be greater than 0", Length.unitless 0., true
      "Expected separation to be greater than -1", Length.unitless -1., true
      "Expected separation to be greater than 0.99", Length.unitless 4.99, true
      "Expected separation to not be greater than 1.01", Length.unitless 5.01, false ]
    |> List.map (fun (name, greaterThan, expected) ->
        TestCaseData(greaterThan)
            .SetName(name)
            .Returns(expected))

[<TestCaseSource(nameof ``Diagonal separation test cases``)>]
let ``separation is determined correctly for diagonally displaced boxes`` (separation: Quantity<Unitless>) =
    let firstBox =
        { MinX = Length.unitless 0.
          MinY = Length.unitless 0.
          MaxX = Length.unitless 1.
          MaxY = Length.unitless 1. }

    let secondBox =
        { MinX = Length.unitless 4.
          MinY = Length.unitless 5.
          MaxX = Length.unitless 5.
          MaxY = Length.unitless 6. }

    BoundingBox2D.separatedByAtLeast separation firstBox secondBox

[<Property>]
let ``offsetBy returns either Nothing or Just a valid box``
    (boundingBox: BoundingBox2D<Meters, TestSpace>)
    (offset: Length)
    =
    match BoundingBox2D.offsetBy offset boundingBox with
    | None -> Test.pass

    | Some result -> Test.isValidBoundingBox2D result


[<Property>]
let ``offsetBy returns either Nothing or Just a valid box when offsetting by -width / 2``
    (boundingBox: BoundingBox2D<Meters, TestSpace>)
    =
    let negativeHalfWidth =
        -0.5 * BoundingBox2D.width boundingBox

    match BoundingBox2D.offsetBy negativeHalfWidth boundingBox with
    | None -> Test.pass
    | Some result -> Test.isValidBoundingBox2D result


[<Property>]
let ``offsetBy returns either Nothing or Just a valid box when offsetting by -height / 2``
    (boundingBox: BoundingBox2D<Meters, TestSpace>)
    =

    let negativeHalfHeight =
        -0.5 * BoundingBox2D.height boundingBox

    match BoundingBox2D.offsetBy negativeHalfHeight boundingBox with
    | None -> Test.pass
    | Some result -> Test.isValidBoundingBox2D result


[<Property>]
let ``hullN is consistent with from`` (first: Point2D<Meters, TestSpace>) (second: Point2D<Meters, TestSpace>) =
    BoundingBox2D.hullN [ first; second ]
    |> Test.equal (Some(BoundingBox2D.from first second))


[<Property>]
let ``hullN does not depend on input order`` (points: Point2D<Meters, TestSpace> list) =
    BoundingBox2D.hullN (List.rev points)
    |> Test.equal (BoundingBox2D.hullN points)
