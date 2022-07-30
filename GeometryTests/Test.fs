module GeometryTests.Test

open System

open Geometry
open Units
open UnitsTests

let validFrame2D (frame: Frame2D<'Unit, 'Coordinates, 'Defines>) : bool =
    let parallelComponent =
        Direction2D.componentIn frame.XDirection frame.YDirection

    if almostEqual parallelComponent 0. then
        true

    else
        printfn
            $"""
Expected perpendicular basis directions, got
Direction: {frame.XDirection}, {frame.YDirection}"
With Parallel Component: {parallelComponent}
        """

        false

let isValidBoundingBox2D (box: BoundingBox2D<'Unit, 'Coordinates>) =
    if box.MinX > box.MaxX then
        Test.fail $"Expected bounding box with extrema to have minX <= maxX.{Environment.NewLine}{box}"
    else if box.MinY > box.MaxY then
        Test.fail "Expected bounding box with extrema to have minY <= maxY.{Environment.NewLine}{box}"
    else
        Test.pass
