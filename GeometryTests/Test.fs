namespace GeometryTests

open System

open Geometry

type TestSpace = TestSpace
type TestDefines = TestDefines

module Test =
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
