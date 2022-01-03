namespace GeometryTests

open Geometry

module Test =

    let equal (expected: float) (actual: float) : bool =
        if expected = actual then
            true
        else
            printf $"Expected: {expected}"
            printf $" But Was: {actual}"
            false

    let almostEqual (expected: float) (actual: float) : bool =
        if abs (expected - actual) < Float.Epsilon then
            true
        else
            printf $"Expected: {expected}"
            printf $" But Was: {actual}"
            false
