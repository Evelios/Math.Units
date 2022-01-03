namespace Utilities

module Test =
    /// Test left and right expecting them to be equal. If they are not equal print out the two values being compared.
    let (.=.) expected actual =
        if expected <> actual then
            printfn $"Expected:\n{expected}\n\n But Was:\n{actual}"

        expected = actual
