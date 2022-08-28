module Math.Units.Tests.Quantity

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Math.Units

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

//---- Basics ------------------------------------------------------------------

[<Test>]
let ``Unitless initialization`` () =
    let expected: Quantity<Unitless> =
        Quantity 1.

    let actual: Quantity<Unitless> =
        Quantity.unitless 1.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Quantity string representation`` () =
    let expected = "1 Meters"

    let actual =
        (Quantity<Meters> 1.).ToString()

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Unsafe construction`` () =
    let expected: Quantity<Unitless> =
        Quantity.create 1.

    let actual: Quantity<Unitless> = Quantity 1.

    Assert.AreEqual(expected, actual)

[<Test>]
let Unwrap () =
    let given: Quantity<Unitless> = Quantity 1.
    let expected: float = 1.
    let actual: float = Quantity.unwrap given

    Assert.AreEqual(expected, actual)


[<Test>]
let Infinity () =
    let expected = Quantity.infinity
    let actual = Quantity 1. / 0.

    Assert.AreEqual(expected, actual)


[<Test>]
let ``Negative Infinity`` () =
    let expected = Quantity.negativeInfinity
    let actual = Quantity -1. / 0.

    Assert.AreEqual(expected, actual)

// ---- Operators --------------------------------------------------------------

[<Test>]
let Equality () =
    let first = Quantity 1.
    let second = Quantity 1.

    Assert.AreEqual(first, second)

[<Test>]
let Inequality () =
    let first = Quantity 1.
    let second = Quantity -1.

    Assert.AreNotEqual(first, second)

[<Test>]
let ``Inequality with other object`` () =
    let first = Quantity 1.
    let second = obj

    Assert.AreNotEqual(first, second)

[<Property>]
let ``Equal with self`` (x: float) =
    let quantity = Quantity x

    Assert.AreEqual(quantity, quantity)

[<Test>]
let ``Approximate equality`` () =
    let first = Quantity 1.
    let second = Quantity 1.00000000001
    let notEqual = Quantity 1.000000001

    Assert.AreEqual(first, second)
    Assert.AreNotEqual(first, notEqual)


[<Test>]
let ``Comparison Operators`` () =
    let lower = Quantity 1.
    let higher = Quantity 3.

    Assert.True(lower < higher)
    Assert.True(higher > lower)

[<Test>]
let Comparison () =
    let lower = Quantity 1.
    let higher = Quantity 3.

    Assert.AreEqual(lower.Comparison(lower), 0)
    Assert.AreEqual(lower.Comparison(higher), -1)
    Assert.AreEqual(higher.Comparison(lower), 1)

    Assert.AreEqual(lower.Comparison(lower), Quantity.compare lower lower)
    Assert.AreEqual(lower.Comparison(higher), Quantity.compare lower higher)
    Assert.AreEqual(higher.Comparison(lower), Quantity.compare higher lower)

[<Property>]
let ``Equal Within`` (quantity: Quantity<Unitless>) (tolerance: Quantity<Unitless>) (ZeroToOne offset) =
    let offsetQuantity: Quantity<Unitless> =
        Quantity.interpolateFrom -tolerance tolerance offset
        |> Quantity.plus quantity

    Test.isTrue
        $"Quantities should be within each other under a tolerance of {tolerance} when offset by {offsetQuantity}"
        (Quantity.equalWithin tolerance quantity offsetQuantity)



//---- Built-in Functions ------------------------------------------------------

[<Test>]
let Abs () =
    let actual = Quantity.abs (Quantity -1.)
    let expected = Quantity 1.
    Assert.AreEqual(expected, actual)

[<Test>]
let Min () =
    let first = Quantity 1.
    let second = Quantity 3.
    let actual = min first second
    let expected = first
    Assert.AreEqual(expected, actual)

[<Test>]
let Max () =
    let first = Quantity 1.
    let second = Quantity 3.
    let actual = max first second
    let expected = second
    Assert.AreEqual(expected, actual)

[<Test>]
let Sqrt () =
    let initial: Quantity<Meters Squared> =
        Quantity 4.

    let actual: Quantity<Meters> =
        Quantity.sqrt initial

    let expected: Quantity<Meters> = Quantity 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Floor () =
    let initial = Quantity 1.5
    let actual = floor initial
    let expected = Quantity 1.
    Assert.AreEqual(expected, actual)

[<Test>]
let Ceiling () =
    let initial = Quantity 1.5
    let actual = ceil initial
    let expected = Quantity 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let Round () =
    let initial = Quantity 1.5
    let actual = Quantity.round initial
    let expected = Quantity 2.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Round To`` () =
    let initial = Quantity 0.123456
    let actual = Quantity.roundTo 3 initial
    let expected = Quantity 0.123
    Assert.AreEqual(expected, actual)

[<Test>]
let Truncate () =
    let initial = Quantity -1.5
    let actual = truncate initial
    let expected = Quantity -1.
    Assert.AreEqual(expected, actual)


//---- Operators ---------------------------------------------------------------

[<Test>]
let Negation () =
    let actual = -Quantity 1.
    let expected = Quantity -1.
    Assert.AreEqual(actual, expected)

[<Test>]
let Addition () =
    let actual = Quantity 1. + Quantity 2.
    let expected = Quantity 3.
    Assert.AreEqual(actual, expected)

[<Test>]
let Subtraction () =
    let actual = Quantity 3. - Quantity 2.
    let expected = Quantity 1.

    Assert.AreEqual(actual, expected)

[<Test>]
let ``Multiplication by float`` () =
    let quantity = Quantity<Unitless> 2.
    let scale = 3.
    let expected = Quantity<Unitless> 6.
    Assert.AreEqual(expected, quantity * scale)
    Assert.AreEqual(expected, scale * quantity)

[<Test>]
let Multiplication () =
    let lhs = Length.meters 3.
    let rhs = Length.meters 4.

    let actual = lhs * rhs

    let expected = Area.squareMeters 12.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Scalar Division`` () =
    let lhs = Quantity 12.
    let rhs = 3.
    let actual = lhs / rhs
    let expected = Quantity 4.
    Assert.AreEqual(expected, actual)

[<Test>]
let Division () =
    let lhs = Quantity 12.
    let rhs = Quantity 3.
    let actual = lhs / rhs
    let expected = 4.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Unit Division`` () =
    let lhs: Quantity<Meters> = Quantity 12.
    let rhs: Quantity<Pixels> = Quantity 3.

    let actual: Quantity<Rate<Meters, Pixels>> =
        lhs / rhs

    let expected: Quantity<Rate<Meters, Pixels>> =
        Quantity 4.

    Assert.AreEqual(expected, actual)

[<Test>]
let Rate () =
    let actual =
        Quantity.rate (Quantity<Meters> 20.) (Quantity<Seconds> 2.)

    let expected = Quantity<MetersPerSecond> 10.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rate Per`` () =
    let actual =
        (Quantity<Meters> 20.)
        |> Quantity.per (Quantity<Seconds> 2.)

    let expected = Quantity<MetersPerSecond> 10.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rate of change at`` () =
    let actual =
        (Quantity<Seconds> 100.)
        |> Quantity.at (Quantity<MetersPerSecond> 2.)

    let expected = Quantity<Meters> 200.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rate of change at inverse`` () =
    let actual =
        (Quantity<Meters> 50.)
        |> Quantity.at_ (Quantity<MetersPerSecond> 2.)

    let expected = Quantity<Seconds> 25.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rate of change for amount`` () =
    let actual =
        (Quantity<MetersPerSecond> 2.)
        |> Quantity.for_ (Quantity<Seconds> 100.)

    let expected = Quantity<Meters> 200.
    Assert.AreEqual(expected, actual)

[<Test>]
let Inverse () =
    let actual =
        Quantity.inverse (Quantity<MetersPerSecond> 100.)

    let expected =
        Quantity<Rate<Seconds, Meters>> 1. / 100.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Rate product`` () =
    let pixelSpeed =
        Quantity<Rate<Pixels, Seconds>> 100.

    let resolution =
        Quantity<Rate<Pixels, Meters>> 1000.
        |> Quantity.inverse

    let pixelSpeedInMeters =
        Quantity.rateProduct pixelSpeed resolution

    let expected =
        Quantity<Rate<Meters, Seconds>> 0.1

    Assert.AreEqual(expected, pixelSpeedInMeters)

[<Test>]
let Modulus () =
    let quantity = Quantity 13.5
    let modulus = Quantity 4.
    let actual = quantity % modulus
    let expected = Quantity 1.5
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Negative Modulus`` () =
    let quantity = Quantity -13.5
    let modulus = Quantity 4.
    let actual = quantity % modulus
    let expected = Quantity -1.5
    Assert.AreEqual(expected, actual)

[<Property>]
let Clamp (first: Quantity<Meters>) (second: Quantity<Meters>) (quantity: Quantity<Meters>) =
    let lower = min first second
    let upper = max first second

    let clamped =
        Quantity.clamp first second quantity

    clamped >= lower && clamped <= upper


[<Test>]
let Squared () =
    let actual: Quantity<Meters Squared> =
        Quantity.squared (Quantity 3.)

    let expected: Quantity<Meters Squared> =
        Quantity 9.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Squared Unitless`` () =
    let actual: Quantity<Unitless> =
        Quantity.squaredUnitless (Quantity 3.)

    let expected: Quantity<Unitless> =
        Quantity 9.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Square Root Unitless`` () =
    let actual: Quantity<Unitless> =
        Quantity.sqrtUnitless (Quantity 9.)

    let expected: Quantity<Unitless> =
        Quantity 3.

    Assert.AreEqual(expected, actual)

[<Test>]
let Cubed () =
    let actual: Quantity<Meters Cubed> =
        Quantity.cubed (Quantity 3.)

    let expected: Quantity<Meters Cubed> =
        Quantity 27.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Cube Root`` () =
    let input: Quantity<Meters Cubed> =
        Quantity 27.

    let actual: Quantity<Meters> =
        Quantity.cbrt input

    let expected: Quantity<Meters> = Quantity 3.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Negative Cube Root`` () =
    let input: Quantity<Meters Cubed> =
        Quantity -27.

    let actual: Quantity<Meters> =
        Quantity.cbrt input

    let expected: Quantity<Meters> =
        Quantity -3.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Cubed Unitless`` () =
    let actual: Quantity<Unitless> =
        Quantity.cubedUnitless (Quantity 3.)

    let expected: Quantity<Unitless> =
        Quantity 27.

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Cube Root Unitless`` () =
    let actual: Quantity<Unitless> =
        Quantity.cbrtUnitless (Quantity 27.)

    let expected: Quantity<Unitless> =
        Quantity 3.

    Assert.AreEqual(expected, actual)

[<Test>]
let Reciprocal () =
    let actual =
        Quantity.reciprocal (Quantity 2.)

    let expected = Quantity 0.5
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Modulus By`` () =
    let actual =
        Quantity.unitless 5.5
        |> Quantity.modBy (Quantity.unitless 2.5)

    let expected = Quantity.unitless 0.5

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Negative Modulus By`` () =
    let actual =
        Quantity.unitless -5.5
        |> Quantity.modBy (Quantity.unitless 2.5)

    let expected = Quantity.unitless -0.5

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Remainder By`` () =
    let actual =
        Quantity.unitless -5.5
        |> Quantity.remainderBy (Quantity.unitless 2.5)

    let expected = Quantity.unitless 0.5

    Assert.AreEqual(expected, actual)

[<Test>]
let Midpoint () =
    let actual =
        Quantity.midpoint (Quantity.unitless 10.) (Quantity.unitless 5)

    let expected = Quantity.unitless 7.5

    Assert.AreEqual(expected, actual)

[<Test>]
let Range () =
    let actual =
        Quantity.range Quantity.zero (Quantity.unitless 10) 4

    let expected =
        [ Quantity.unitless 0.
          Quantity.unitless 2.5
          Quantity.unitless 5.
          Quantity.unitless 7.5
          Quantity.unitless 10 ]

    Assert.AreEqual(expected, actual)

[<Test>]
let ``In particular unit scheme`` () =
    let actual =
        Length.feet 10. |> Quantity.in_ Length.inches

    let expected = 120.

    Assert.AreEqual(expected, actual, Float.Epsilon)

[<Test>]
let ``Range Edge Case`` () =
    let actual: Quantity<Unitless> list =
        Quantity.range Quantity.zero (Quantity.unitless 10) -1

    let expected: Quantity<Unitless> list = []

    Assert.AreEqual(expected, actual)

[<Test>]
let Sum () =
    let actual =
        Quantity.sum [
            Quantity.unitless 1.
            Quantity.unitless 2.
            Quantity.unitless 3.
        ]

    let expected = Quantity.unitless 6.

    Assert.AreEqual(expected, actual)


[<Test>]
let Remainder () =
    let quantity = Quantity 13.5
    let modulus = Quantity 4.

    let actual =
        Quantity.remainderBy modulus quantity

    let expected = Quantity 1.5
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Negative Remainder`` () =
    let quantity = Quantity -13.5
    let modulus = Quantity 4.

    let actual =
        Quantity.remainderBy modulus quantity

    let expected = Quantity 1.5
    Assert.AreEqual(expected, actual)

[<Test>]
let Over () =
    let area = Quantity<SquareMeters> 24.
    let length = Quantity<Meters> 4.

    let actual: Quantity<Meters> =
        area |> Quantity.over length

    let expected = Quantity<Meters> 6.
    Assert.AreEqual(expected, actual)

[<Test>]
let Over_ () =
    let force = Quantity<Newtons> 24.

    let acceleration =
        Quantity<MetersPerSecondSquared> 4.

    let actual: Quantity<Kilograms> =
        force |> Quantity.over_ acceleration

    let expected = Quantity<Kilograms> 6.
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Over Unitless`` () =
    let actual =
        Quantity.unitless 30.
        |> Quantity.overUnitless (Quantity.unitless 6.)

    let expected = Quantity.unitless 5.

    Assert.AreEqual(expected, actual)

[<Test>]
let Ratio () =
    let actual =
        Quantity.ratio (Length.miles 1.) (Length.yards 1.)

    let expected = 1760.

    Assert.AreEqual(expected, actual)

/// ---- Operator Comparison Testing ----

[<Test>]
let ``Less Than Zero`` () =
    Assert.IsTrue(Quantity.unitless -1. |> Quantity.lessThanZero)

[<Test>]
let ``Less Than Or Equal To Zero`` () =
    Assert.IsTrue(
        Quantity.unitless 0.
        |> Quantity.lessThanOrEqualToZero
    )

    Assert.IsTrue(
        Quantity.unitless -1.
        |> Quantity.lessThanOrEqualToZero
    )

[<Test>]
let ``Greater Than Zero`` () =
    Assert.IsTrue(Quantity.unitless 1. |> Quantity.greaterThanZero)

[<Test>]
let ``Greater Than Or Equal To Zero`` () =
    Assert.IsTrue(
        Quantity.unitless 0.
        |> Quantity.greaterThanOrEqualToZero
    )

    Assert.IsTrue(
        Quantity.unitless 1.
        |> Quantity.greaterThanOrEqualToZero
    )

// ---- Operator Comparison ----


// ---- Unary Comparison ---

[<Property>]
let ``Abs Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.abs Quantity.Abs

[<Property>]
let ``Sqrt Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.sqrt Quantity.Sqrt

[<Property>]
let ``Floor Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.floor Quantity.Floor

[<Property>]
let ``Round Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.round Quantity.Round

[<Property>]
let ``Ceiling Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.ceil Quantity.Ceiling

[<Property>]
let ``Truncate Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.truncate Quantity.Truncate

[<Property>]
let ``Negate Comparison`` () =
    Test.unaryOperator Arb.quantity Quantity.negate (fun a -> -a)

[<Property>]
let ``Hash Comparison`` () =
    Test.unaryOperator Arb.quantity hash (fun q -> q.GetHashCode())

[<Property>]
let ``Equality and Hash should be equal`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

// ---- Binary Comparison ----

[<Property>]
let ``Max Comparison`` () =
    Test.binaryOperator Gen.quantity Quantity.max (fun a b -> Quantity.Max(a, b))

[<Property>]
let ``Min Comparison`` () =
    Test.binaryOperator Gen.quantity Quantity.min (fun a b -> Quantity.Min(a, b))

[<Property>]
let ``Less Than`` () =
    Test.binaryOperator Gen.quantity Quantity.lessThan (<)

[<Property>]
let ``Less Than Or Equal`` () =
    Test.binaryOperator Gen.quantity Quantity.lessThanOrEqualTo (<=)

[<Property>]
let ``Greater Than `` () =
    Test.binaryOperator Gen.quantity Quantity.greaterThan (>)

[<Property>]
let ``Greater Than Or Equal`` () =
    Test.binaryOperator Gen.quantity Quantity.greaterThanOrEqualTo (>=)

[<Property>]
let Plus () =
    Test.binaryOperator Gen.quantity Quantity.plus (+)

[<Property>]
let Minus () =
    Test.binaryOperator Gen.quantity Quantity.minus (-)

[<Property>]
let Difference () =
    Test.binaryOperator Gen.quantity (fun y x -> Quantity.difference x y) (-)

[<Property>]
let ``Difference And Minus`` () =
    Test.binaryOperator Gen.quantity Quantity.difference Quantity.minus

[<Property>]
let ``Product and Times`` () =
    Test.binaryOperator Gen.quantity Quantity.product Quantity.times


// ----  Accessors -------------------------------------------------------------

[<Property>]
let ``Is Infinite`` () =
    Assert.IsTrue(Quantity.isInfinite Quantity.positiveInfinity)
    Assert.IsTrue(Quantity.isInfinite Quantity.negativeInfinity)
    Assert.IsTrue(Quantity.isInfinite Quantity.infinity)

[<Property>]
let ``Is NaN`` () =
    Assert.IsTrue(Quantity.isNaN (Quantity nan))

[<Property>]
let ``Minimum quantity of list is the smallest`` (quantities: Quantity<Unitless> list) =
    let maybeMinimum =
        Quantity.minimum quantities

    match maybeMinimum with
    | Some minimum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q >= minimum) quantities

    | None ->
        Test.isTrue
            "Minimum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])

[<Property>]
let ``MinimumBy quantity of list is the smallest`` (quantities: (Quantity<Unitless> * unit) list) =
    let maybeMinimum =
        Quantity.minimumBy fst quantities

    match maybeMinimum with
    | Some minimum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q >= minimum) quantities

    | None ->
        Test.isTrue
            "Minimum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])

[<Property>]
let ``Maximum quantity of list is the smallest`` (quantities: Quantity<Unitless> list) =
    let maybeMaximum =
        Quantity.maximum quantities

    match maybeMaximum with
    | Some maximum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q <= maximum) quantities

    | None ->
        Test.isTrue
            "Maximum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])

[<Property>]
let ``MaximumBy quantity of list is the smallest`` (quantities: (Quantity<Unitless> * unit) list) =
    let maybeMaximum =
        Quantity.maximumBy fst quantities

    match maybeMaximum with
    | Some maximum ->
        Test.forAll (fun q -> $"The quantity {q} is not the smallest in the list") (fun q -> q >= maximum) quantities

    | None ->
        Test.isTrue
            "Minimum should only be an empty list when the list of quantities is also an empty list."
            (quantities = [])

[<Property>]
let Sort (quantities: Quantity<Unitless> list) =
    let sorted = Quantity.sort quantities

    List.pairwise sorted
    |> List.fold (fun wellSorted (first, second) -> first <= second && wellSorted) true

[<Property>]
let SortBy (quantities: (Quantity<Unitless> * unit) list) =
    let sorted = Quantity.sortBy fst quantities

    List.pairwise sorted
    |> List.fold (fun wellSorted (first, second) -> first <= second && wellSorted) true
