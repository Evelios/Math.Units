namespace UnitsTests

open System

open Units

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
        
    let forAll (message: 'a -> string) (test: 'a -> bool) (values : 'a list): bool =
        let mutable testPasses = true
        
        for value in values do
            if not <| test value then
                printfn $"{message value}"
                testPasses <- false
                
        testPasses

    let private comparison lhs operator rhs name : bool =
        if operator rhs lhs then
            true
        else
            printfn name
            false

    let equal expected actual : bool =
        comparison expected (=) actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

    let almostEqual expected actual : bool =
        comparison expected Float.almostEqual actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

    let lessThan expected actual : bool =
        comparison expected (<) actual $"Expected: {expected} < {actual}"

    let lessThanOrEqualTo expected actual : bool =
        comparison expected (<=) actual $"Expected: {expected} <= {actual}"

    let greaterThan expected actual : bool =
        comparison expected (>) actual $"Expected: {expected} > {actual}"

    let greaterThanOrEqualTo expected actual : bool =
        comparison expected (>=) actual $"Expected: {expected} >= {actual}"

    let all tests = List.forall id tests



[<AutoOpen>]
module Operators =
    let (.=.) lhs rhs = lhs |> Test.equal rhs
    let (.==.) lhs rhs = lhs |> Test.almostEqual rhs
    let (.>.) lhs rhs = lhs |> Test.greaterThan rhs
    let (.>=.) lhs rhs = lhs |> Test.greaterThanOrEqualTo rhs
    let (.<.) lhs rhs = lhs |> Test.lessThan rhs
    let (.<=.) lhs rhs = lhs |> Test.lessThanOrEqualTo rhs
