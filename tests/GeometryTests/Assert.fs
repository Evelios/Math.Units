module GeometryTests.Assert

open NUnit.Framework

open Geometry

let AlmostEqual expected actual : unit =
    if Float.almostEqual expected actual then
        Assert.Pass()
    else
        Assert.Fail(
            $"""
            Expected: {expected}
            But Was: {actual}
            """
        )
