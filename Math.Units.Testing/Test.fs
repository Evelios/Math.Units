namespace Math.Units.Test


/// <summary>Type for test space tagging.</summary>
type TestSpace = TestSpace
/// <summary>Type for test definitions tagging.</summary>
type TestDefines = TestDefines

/// <summary>Shared testing functionality for Math.Units.</summary>
module Test =
    open System
    open FsCheck

    open Math.Units
    open FSharp.Extensions

    /// <summary>Constant for passing tests.</summary>
    [<Literal>]
    let pass = true

    /// <summary>Fail a test with a message.</summary>
    /// <param name="failMessage">The message to print.</param>
    let fail failMessage =
        printfn $"{failMessage}"
        false

    /// <summary>Assert that a condition is false.</summary>
    /// <param name="failMessage">The message to print if assertion fails.</param>
    /// <param name="assertion">The condition to check.</param>
    let isFalse failMessage assertion =
        if assertion then
            printfn $"{failMessage}"

        not assertion

    /// <summary>Assert that a condition is true.</summary>
    /// <param name="failMessage">The message to print if assertion fails.</param>
    /// <param name="assertion">The condition to check.</param>
    let isTrue failMessage assertion =
        if not assertion then
            printfn $"{failMessage}"

        assertion

    /// <summary>Assert that a test holds for all values in a list.</summary>
    /// <param name="message">A function to generate a fail message for a value.</param>
    /// <param name="test">The test function.</param>
    /// <param name="values">The list of values to test.</param>
    let forAll (message: 'a -> string) (test: 'a -> bool) (values: 'a list) : bool =
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

    /// <summary>Assert that two values are equal.</summary>
    /// <param name="expected">The expected value.</param>
    /// <param name="actual">The actual value.</param>
    let equal expected actual : bool =
        comparison expected (=) actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

    /// <summary>Assert that two values are approximately equal.</summary>
    /// <param name="expected">The expected value.</param>
    /// <param name="actual">The actual value.</param>
    let almostEqual expected actual : bool =
        let getValue x =
            match box x with
            | :? float as f -> f
            | :? IQuantity as q -> q.Value
            | :? Temperature as t -> t.Value
            | :? bool as b -> if b then 1.0 else 0.0
            | _ -> failwith $"Cannot compare {x.GetType()} approximately"

        let v1 = getValue expected
        let v2 = getValue actual
        
        if Float.almostEqual v1 v2 then
            true
        else
            printfn $"Expected approximately: {expected}{Environment.NewLine} But Was: {actual}"
            false

    /// <summary>Assert that actual is less than expected.</summary>
    /// <param name="expected">The limit value.</param>
    /// <param name="actual">The value to check.</param>
    let lessThan expected actual : bool =
        comparison expected (<) actual $"Expected: {expected} < {actual}"

    /// <summary>Assert that actual is less than or equal to expected.</summary>
    /// <param name="expected">The limit value.</param>
    /// <param name="actual">The value to check.</param>
    let lessThanOrEqualTo expected actual : bool =
        comparison expected (<=) actual $"Expected: {expected} <= {actual}"

    /// <summary>Assert that actual is greater than expected.</summary>
    /// <param name="expected">The limit value.</param>
    /// <param name="actual">The value to check.</param>
    let greaterThan expected actual : bool =
        comparison expected (>) actual $"Expected: {expected} > {actual}"

    /// <summary>Assert that actual is greater than or equal to expected.</summary>
    /// <param name="expected">The limit value.</param>
    /// <param name="actual">The value to check.</param>
    let greaterThanOrEqualTo expected actual : bool =
        comparison expected (>=) actual $"Expected: {expected} >= {actual}"

    /// <summary>Check if all booleans in a list are true.</summary>
    /// <param name="tests">The list of results.</param>
    let all tests = List.forall id tests

    /// <summary>Verify a unary operator property.</summary>
    /// <param name="arb">The Arbitrary instance for the input.</param>
    /// <param name="fn">The function to verify.</param>
    /// <param name="op">The reference operator.</param>
    let unaryOperator (arb: Arbitrary<'a>) (fn: 'a -> 'b) (op: 'a -> 'b) : Property =
        Prop.forAll arb (fun q -> almostEqual (fn q) (op q))


    /// <summary>Verify a binary operator property.</summary>
    /// <param name="arb">The generator for the input.</param>
    /// <param name="fn">The function to verify.</param>
    /// <param name="op">The reference operator.</param>
    let binaryOperator (arb: Gen<'a>) (fn: 'a -> 'a -> 'b) (op: 'a -> 'a -> 'b) : Property =
        let arb =
            Gen.map2 (fun a b -> a, b) arb arb |> Arb.fromGen

        Prop.forAll arb (fun (a, b) -> almostEqual (fn a b) (a |> op b))


/// <summary>Custom operators for testing.</summary>
[<AutoOpen>]
module Operators =
    /// <summary>Approximate equality operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.=.) lhs rhs = lhs |> Test.almostEqual rhs
    /// <summary>Approximate equality operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.==.) lhs rhs = lhs |> Test.almostEqual rhs
    /// <summary>Greater than operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.>.) lhs rhs = lhs |> Test.greaterThan rhs
    /// <summary>Greater than or equal to operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.>=.) lhs rhs = lhs |> Test.greaterThanOrEqualTo rhs
    /// <summary>Less than operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.<.) lhs rhs = lhs |> Test.lessThan rhs
    /// <summary>Less than or equal to operator.</summary>
    /// <param name="lhs">The left hand side.</param>
    /// <param name="rhs">The right hand side.</param>
    let (.<=.) lhs rhs = lhs |> Test.lessThanOrEqualTo rhs
