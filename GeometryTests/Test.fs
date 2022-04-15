namespace GeometryTests

open System

open Geometry

type TestSpace = TestSpace
type TestDefines = TestDefines

module Test =
    [<Literal>]
    let pass = true

    let fail failMessage =
        printfn $"{failMessage}"
        false

    let isFalse failMessage assertion =
        if assertion then
            printfn $"{failMessage}"

        not assertion

    let isTrue failMessage assertion =
        if not assertion then
            printfn $"{failMessage}"

        assertion

    let private comparison name operator lhs rhs : bool =
        if operator rhs lhs then
            true
        else
            printf $"{name}: {lhs}{Environment.NewLine}"
            printf $" But Was: {rhs}{Environment.NewLine}"
            false

    let equal expected actual : bool =
        comparison "Expected" (=) expected actual

    let almostEqual expected actual : bool =
        comparison "Expected" almostEqual expected actual

    let lessThan expected actual : bool =
        comparison "Expected to be less than" (<) expected actual

    let lessThanOrEqualTo expected actual : bool =
        comparison "Expected to be less than or equal to" (<=) expected actual

    let greaterThan expected actual : bool =
        comparison "Expected to be greater than" (>) expected actual

    let greaterThanOrEqualTo expected actual : bool =
        comparison "Expected to be greater than or equal to" (>=) expected actual

    let all tests = List.forall id tests

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
            fail $"Expected bounding box with extrema to have minX <= maxX.{Environment.NewLine}{box}"
        else if box.MinY > box.MaxY then
            fail "Expected bounding box with extrema to have minY <= maxY.{Environment.NewLine}{box}"
        else
            pass
