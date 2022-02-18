namespace GeometryTests

type TestSpace = TestSpace

module Test =
    let equal expected actual : bool =
        if expected = actual then
            true
        else
            printf $"Expected: {expected}"
            printf $" But Was: {actual}\n"
            false