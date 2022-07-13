module UnitsTests

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units

[<SetUp>]
let Setup () = ()


//---- Comparison --------------------------------------------------------------

[<Test>]
let Equality () =
    let first = Quantity 1.
    let second = Quantity 1.

    Assert.AreEqual(first, second)

[<Test>]
let FuzzyEquality () =
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


//---- Built-in Functions ------------------------------------------------------

[<Test>]
let Abs () =
    let actual = abs (Quantity -1.)
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
    let initial : Quantity<Meters Squared> = Quantity 4.
    let actual : Quantity<Meters> = Quantity.sqrt initial
    let expected : Quantity<Meters> = Quantity 2.
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
    let lhs = Quantity 3.
    let rhs = Quantity 4.
    let actual : Quantity<Meters Squared> = lhs * rhs
    let expected : Quantity<Meters Squared> = Quantity 12.

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
    let lhs : Quantity<Meters> = Quantity 12.
    let rhs : Quantity<Pixels> = Quantity 3.
    let actual : Quantity<Rate<Meters, Pixels>> = lhs / rhs
    let expected : Quantity<Rate<Meters, Pixels>> = Quantity 4.
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
