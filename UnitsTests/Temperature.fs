module UnitsTests.Temperature

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Operators --------------------------------------------------------------

[<Test>]
let Equality () =
    let first = Temperature 1.
    let second = Temperature 1.

    Assert.AreEqual(first, second)

[<Test>]
let Inequality () =
    let first = Temperature 1.
    let second = Temperature -1.

    Assert.AreNotEqual(first, second)

[<Test>]
let ``Inequality with other object`` () =
    let first = Temperature 1.
    let second = obj

    Assert.AreNotEqual(first, second)

[<Property>]
let ``Equal with self`` (x: float) =
    let quantity = Temperature x

    Assert.AreEqual(quantity, quantity)

[<Test>]
let ``Approximate equality`` () =
    let first = Temperature 1.
    let second = Temperature 1.00000000001
    let notEqual = Temperature 1.000000001

    Assert.AreEqual(first, second)
    Assert.AreNotEqual(first, notEqual)


[<Test>]
let ``Comparison Operators`` () =
    let lower = Temperature 1.
    let higher = Temperature 3.

    Assert.True(lower < higher)
    Assert.True(higher > lower)

[<Test>]
let Comparison () =
    let lower = Temperature 1.
    let higher = Temperature 3.

    Assert.AreEqual(lower.Comparison(lower), 0)
    Assert.AreEqual(lower.Comparison(higher), -1)
    Assert.AreEqual(higher.Comparison(lower), 1)


//---- Built-in Functions ------------------------------------------------------

[<Test>]
let Abs () =
    let actual =
        Temperature.abs (Temperature -1.)

    let expected = Temperature 1.
    Assert.AreEqual(expected, actual)

[<Test>]
let Min () =
    let first = Temperature 1.
    let second = Temperature 3.
    let actual = min first second
    let expected = first
    Assert.AreEqual(expected, actual)

[<Test>]
let Max () =
    let first = Temperature 1.
    let second = Temperature 3.
    let actual = max first second
    let expected = second
    Assert.AreEqual(expected, actual)


[<Test>]
let Floor () =
    let initial = Temperature 1.5
    let actual = floor initial
    let expected = Temperature 1.
    Assert.AreEqual(expected, actual)

[<Test>]
let Ceiling () =
    let initial = Temperature 1.5
    let actual = ceil initial
    let expected = Temperature 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Round () =
    let initial = Temperature 1.5
    let actual = Temperature.round initial
    let expected = Temperature 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Truncate () =
    let initial = Temperature -1.5
    let actual = truncate initial
    let expected = Temperature -1.
    Assert.AreEqual(expected, actual)

// ---- Operators --------------------------------------------------------------

[<Test>]
let Addition () =
    let temp = Temperature 1.
    let delta = Delta 2.
    let expected = Temperature 3.

    Assert.AreEqual(expected, temp + delta)
    Assert.AreEqual(expected, delta + temp)

[<Test>]
let Subtraction () =
    let actual = Temperature 3. - Temperature 2.
    let expected = Delta 1.

    Assert.AreEqual(actual, expected)


// ---- Binary Comparison ----

[<Property>]
let ``Max Comparison`` () =
    Test.binaryOperator Gen.temperature Temperature.max (fun a b -> Temperature.Max(a, b))

[<Property>]
let ``Min Comparison`` () =
    Test.binaryOperator Gen.temperature Temperature.min (fun a b -> Temperature.Min(a, b))

[<Property>]
let ``Less Than`` () =
    Test.binaryOperator Gen.temperature Temperature.lessThan (<)

[<Property>]
let ``Less Than Or Equal`` () =
    Test.binaryOperator Gen.temperature Temperature.lessThanOrEqualTo (<=)

[<Property>]
let ``Greater Than `` () =
    Test.binaryOperator Gen.temperature Temperature.greaterThan (>)

[<Property>]
let ``Greater Than Or Equal`` () =
    Test.binaryOperator Gen.temperature Temperature.greaterThanOrEqualTo (>=)


// ---- Functions ----


[<Property>]
let ``Minimum quantity of list is the smallest`` (quantities: Temperature list) =
    let maybeMinimum =
        Temperature.minimum quantities

    match maybeMinimum with
    | Some minimum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q >= minimum) quantities

    | None ->
        Test.isTrue
            "Minimum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])


[<Property>]
let ``Maximum quantity of list is the smallest`` (quantities: Temperature list) =
    let maybeMaximum =
        Temperature.maximum quantities

    match maybeMaximum with
    | Some maximum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q <= maximum) quantities

    | None ->
        Test.isTrue
            "Maximum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])


[<Property>]
let Sort (quantities: Temperature list) =
    let sorted = Temperature.sort quantities

    List.pairwise sorted
    |> List.fold (fun wellSorted (first, second) -> first <= second && wellSorted) true


[<Property>]
let SortBy (quantities: (Temperature * unit) list) =
    let sorted =
        Temperature.sortBy fst quantities

    List.pairwise sorted
    |> List.fold (fun wellSorted (first, second) -> first <= second && wellSorted) true


[<Property>]
let Clamp (first: Temperature) (second: Temperature) (quantity: Temperature) =
    let lower = min first second
    let upper = max first second

    let clamped =
        Temperature.clamp first second quantity

    clamped >= lower && clamped <= upper
