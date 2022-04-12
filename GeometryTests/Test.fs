namespace GeometryTests

open System

open Geometry

type TestSpace = TestSpace
type TestDefines = TestDefines

module Test =
    let equal expected actual : bool =
        if expected = actual then
            true
        else
            printf $"Expected: {expected}{Environment.NewLine}"
            printf $" But Was: {actual}{Environment.NewLine}"
            false

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
