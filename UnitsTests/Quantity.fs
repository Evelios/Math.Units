module UnitsTests.Quantity

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units
open FSharp.Extensions

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
let ``Unsafe construction`` () =
    let expected: Quantity<Unitless> =
        Quantity.unsafe 1.

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

// ---- Operators ----

[<Test>]
let Equality () =
    let first = Quantity 1.
    let second = Quantity 1.

    Assert.AreEqual(first, second)

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
    let actual = round initial
    let expected = Quantity 2.
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
    let quantity = Quantity 1.
    let scale = 3.
    let expected = Quantity 3.
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
        Quantity.clamp lower upper quantity

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

let testUnaryOperator (fn: Quantity<'Unit> -> 'a) (op: Quantity<'Unit> -> 'a) : Property =
    Prop.forAll Arb.quantity (fun q -> Test.equal (fn q) (op q))


let testBinaryOperator
    (fn: Quantity<'Unit> -> Quantity<'Unit> -> 'a)
    (op: Quantity<'Unit> -> Quantity<'Unit> -> 'a)
    : Property =

    let arb =
        Gen.map2 Tuple2.pair Gen.quantity Gen.quantity
        |> Arb.fromGen

    Prop.forAll arb (fun (a, b) -> Test.equal (fn a b) (a |> op b))

// ---- Unary Comparison ---

[<Property>]
let ``Abs Comparison`` () =
    testUnaryOperator Quantity.abs Quantity.Abs

[<Property>]
let ``Negate Comparison`` () =
    testUnaryOperator Quantity.negate (fun a -> -a)

// ---- Binary Comparison ----

[<Property>]
let ``Max Comparison`` () =
    testBinaryOperator Quantity.max (fun a b -> Quantity.Max(a, b))

[<Property>]
let ``Min Comparison`` () =
    testBinaryOperator Quantity.min (fun a b -> Quantity.Min(a, b))

[<Property>]
let ``Less Than`` () =
    testBinaryOperator Quantity.lessThan (<)

[<Property>]
let ``Less Than Or Equal`` () =
    testBinaryOperator Quantity.lessThanOrEqualTo (<=)

[<Property>]
let ``Greater Than `` () =
    testBinaryOperator Quantity.greaterThan (>)

[<Property>]
let ``Greater Than Or Equal`` () =
    testBinaryOperator Quantity.greaterThanOrEqualTo (>=)

[<Property>]
let Minus () = testBinaryOperator Quantity.minus (-)

[<Property>]
let Difference () =
    testBinaryOperator (fun y x -> Quantity.difference x y) (-)

[<Property>]
let ``Difference And Minus`` () =
    testBinaryOperator Quantity.difference Quantity.minus

[<Property>]
let ``Product and Times`` () =
    testBinaryOperator Quantity.product Quantity.times


// ----  Accessors -------------------------------------------------------------

[<Property>]
let ``Is Infinite`` () =
    Assert.IsTrue(Quantity.isInfinite Quantity.positiveInfinity)
    Assert.IsTrue(Quantity.isInfinite Quantity.negativeInfinity)
    Assert.IsTrue(Quantity.isInfinite Quantity.infinity)

[<Property>]
let ``Is NaN`` () =
    Assert.IsTrue(Quantity.isNaN (Quantity nan))
