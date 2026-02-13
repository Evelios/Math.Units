namespace Math.Units.Test


type TestSpace = TestSpace
type TestDefines = TestDefines

module Test =
    open System
    open FsCheck

    open Math.Units
    open FSharp.Extensions

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

    let equal expected actual : bool =
        comparison expected (=) actual $"Expected: {expected}{Environment.NewLine} But Was: {actual}"

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

    let lessThan expected actual : bool =
        comparison expected (<) actual $"Expected: {expected} < {actual}"

    let lessThanOrEqualTo expected actual : bool =
        comparison expected (<=) actual $"Expected: {expected} <= {actual}"

    let greaterThan expected actual : bool =
        comparison expected (>) actual $"Expected: {expected} > {actual}"

    let greaterThanOrEqualTo expected actual : bool =
        comparison expected (>=) actual $"Expected: {expected} >= {actual}"

    let all tests = List.forall id tests

    let unaryOperator (arb: Arbitrary<'a>) (fn: 'a -> 'b) (op: 'a -> 'b) : Property =
        Prop.forAll arb (fun q -> almostEqual (fn q) (op q))


    let binaryOperator (arb: Gen<'a>) (fn: 'a -> 'a -> 'b) (op: 'a -> 'a -> 'b) : Property =
        let arb =
            Gen.map2 Tuple2.pair arb arb |> Arb.fromGen

        Prop.forAll arb (fun (a, b) -> almostEqual (fn a b) (a |> op b))


[<AutoOpen>]
module Operators =
    let (.=.) lhs rhs = lhs |> Test.almostEqual rhs
    let (.==.) lhs rhs = lhs |> Test.almostEqual rhs
    let (.>.) lhs rhs = lhs |> Test.greaterThan rhs
    let (.>=.) lhs rhs = lhs |> Test.greaterThanOrEqualTo rhs
    let (.<.) lhs rhs = lhs |> Test.lessThan rhs
    let (.<=.) lhs rhs = lhs |> Test.lessThanOrEqualTo rhs
