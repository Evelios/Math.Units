module GeometryTests.Direction2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open UnitsTests
open Geometry
open Units


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Property>]
let ``angleFrom And equalWithin Are Consistent`` (first: Direction2D<TestSpace>) (second: Direction2D<TestSpace>) =
    let angle =
        Angle.abs (Direction2D.angleFrom first second)

    let tolerance = angle + Angle.radians Float.Epsilon

    Direction2D.equalWithin tolerance first second


[<Property>]
let ``angleFrom And rotateBy Are Consistent`` (first: Direction2D<TestSpace>) (second: Direction2D<TestSpace>) =
    let angle = Direction2D.angleFrom first second

    first
    |> Direction2D.rotateBy angle
    |> Test.equal second


[<Property>]
let ``orthonormalize Produces a Valid Frame Basis``
    (vx: Vector2D<Meters, TestSpace>)
    (vy: Vector2D<Meters, TestSpace>)
    =

    match Direction2D.orthonormalize vx vy with

    | Some (xDirection, yDirection) ->
        Test.validFrame2D
            { Origin = Point2D.origin
              XDirection = xDirection
              YDirection = yDirection }

    | None ->
        Vector2D.cross vx vy
        |> Test.equal Quantity.zero


[<Property>]
let ``rotateClockwise Is Consistent With rotateBy`` (direction: Direction2D<TestSpace>) =
    Direction2D.rotateClockwise direction
    |> Test.equal (Direction2D.rotateBy -Angle.halfPi direction)


[<Property>]
let ``rotateCounterClockwise Is Consistent With rotateBy`` (direction: Direction2D<TestSpace>) =
    Direction2D.rotateCounterclockwise direction
    |> Test.equal (Direction2D.rotateBy Angle.halfPi direction)


[<Property>]
let ``fromAngle Is Consistent With angleFrom`` (direction: Direction2D<TestSpace>) =
    Direction2D.angleFrom Direction2D.x direction
    |> Direction2D.fromAngle
    |> Test.equal direction


[<Property>]
let ``fromAngle Is Consistent With toAngle`` (direction: Direction2D<TestSpace>) =
    Direction2D.toAngle direction
    |> Direction2D.fromAngle
    |> Test.equal direction


[<Property>]
let ``componentIn Is Consistent With xComponent Component`` (direction: Direction2D<TestSpace>) =
    Direction2D.componentIn Direction2D.x direction
    |> Test.equal (Direction2D.xComponent direction)


[<Property>]
let ``componentIn Is Consistent With yComponent Component`` (direction: Direction2D<TestSpace>) =
    Direction2D.componentIn Direction2D.y direction
    |> Test.equal (Direction2D.yComponent direction)


[<Property>]
let ``Mirroring twice returns original direction``
    (direction: Direction2D<TestSpace>)
    (axis: Axis2D<Meters, TestSpace>)
    =
    direction
    |> Direction2D.mirrorAcross axis
    |> Direction2D.mirrorAcross axis
    |> Test.equal direction


[<Property>]
let ``Mirroring negates angle from axis`` (direction: Direction2D<TestSpace>) (axis: Axis2D<Meters, TestSpace>) =
    let mirroredDirection = Direction2D.mirrorAcross axis direction

    let originalAngle =
        Direction2D.angleFrom axis.Direction direction

    let mirroredAngle =
        Direction2D.angleFrom axis.Direction mirroredDirection

    Test.equal -originalAngle mirroredAngle


[<Property>]
let ``relativeTo and placeIn are inverses``
    (direction: Direction2D<TestSpace>)
    (frame: Frame2D<Meters, TestSpace, TestDefines>)
    =
    direction
    |> Direction2D.relativeTo frame
    |> Direction2D.placeIn frame
    |> Test.equal direction


[<Property>]
let ``components and xComponents/yComponents are consistent`` (direction: Direction2D<TestSpace>) =
    Test.equal (Direction2D.components direction) (Direction2D.xComponent direction, Direction2D.yComponent direction)



// ---- Equal Within ----

[<Property>]
let ``Rotation by 2 degrees`` (direction: Direction2D<TestSpace>) =
    let rotatedDirection =
        Direction2D.rotateBy (Angle.degrees 2.) direction

    Direction2D.equalWithin (Angle.degrees 3.) direction rotatedDirection
    && not (Direction2D.equalWithin (Angle.degrees 1.) direction rotatedDirection)


[<Property>]
let ``Rotation by 90 degrees`` (direction: Direction2D<TestSpace>) =
    let rotatedDirection =
        Direction2D.rotateBy (Angle.degrees 90.) direction

    Direction2D.equalWithin (Angle.degrees 91.) direction rotatedDirection
    && not (Direction2D.equalWithin (Angle.degrees 89.) direction rotatedDirection)


[<Property>]
let ``Rotation by 178 degrees`` (direction: Direction2D<TestSpace>) =
    let rotatedDirection =
        Direction2D.rotateBy (Angle.degrees 178.) direction

    Direction2D.equalWithin (Angle.degrees 179.) direction rotatedDirection
    && not (Direction2D.equalWithin (Angle.degrees 177.) direction rotatedDirection)


[<Property>]
let ``All directions are equal within 180 degrees`` (first: Direction2D<TestSpace>) (second: Direction2D<TestSpace>) =
    Direction2D.equalWithin (Angle.degrees 180.000000001) first second
