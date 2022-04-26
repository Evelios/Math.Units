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

    let private comparison lhs operator rhs name : bool =
        if operator rhs lhs then
            true
        else
            printfn name
            false

    let equal expected actual : bool =
        comparison expected (=) actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

    let almostEqual expected actual : bool =
        comparison expected almostEqual actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

    let lessThan expected actual : bool =
        comparison expected (<) actual $"Expected: {expected} < {actual}"

    let lessThanOrEqualTo expected actual : bool =
        comparison expected (<=) actual $"Expected: {expected} <= {actual}"

    let greaterThan expected actual : bool =
        comparison expected (>) actual $"Expected: {expected} > {actual}"

    let greaterThanOrEqualTo expected actual : bool =
        comparison expected (>=) actual $"Expected: {expected} >= {actual}"

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


[<AutoOpen>]
module Operators =
    let (.=.) lhs rhs = lhs |> Test.equal rhs
    let (.==.) lhs rhs = lhs |> Test.almostEqual rhs
    let (.>.) lhs rhs = lhs |> Test.greaterThan rhs
    let (.>=.) lhs rhs = lhs |> Test.greaterThanOrEqualTo rhs
    let (.<.) lhs rhs = lhs |> Test.lessThan rhs
    let (.<=.) lhs rhs = lhs |> Test.lessThanOrEqualTo rhs
