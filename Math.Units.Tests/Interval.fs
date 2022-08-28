module Math.Units.Tests.Interval

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Math.Units


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Test inequality with other object type`` () =
    let interval =
        Interval.from (Quantity.unitless 1.) (Quantity.unitless 1.)

    let other = obj

    Assert.AreNotEqual(interval, other)

[<Test>]
let ``Interval string`` () =
    let interval =
        Interval.from (Length.meters 1.5) (Length.meters 2.)

    let expected =
        "Interval [ 1.5 Meters -> 2 Meters ]"

    Assert.AreEqual(expected, interval.ToString())


[<Property>]
let ``Equality and Hash should be equal`` (first: Interval<Unitless>) (second: Interval<Unitless>) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())


[<Property>]
let ``from is equal to fromEndpoints`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    Test.equal (Interval.from first second) (Interval.fromEndpoints (first, second))


[<Property>]
let ``Endpoints are equal to input endpoints`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    let lower = min first second
    let upper = max first second
    let endpoints = lower, upper

    Interval.fromEndpoints endpoints
    |> Interval.endpoints
    |> Test.equal endpoints

[<Property>]
let ``Min value of interval`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    let lower = min first second

    Interval.fromEndpoints (first, second)
    |> Interval.minValue
    |> Test.equal lower

[<Property>]
let ``Max value of interval`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    let upper = max first second

    Interval.fromEndpoints (first, second)
    |> Interval.maxValue
    |> Test.equal upper

[<Property>]
let ``Midpoint is equidistant from endpoints`` (first: Quantity<Unitless>) (second: Quantity<Unitless>) =
    let midpoint =
        Interval.fromEndpoints (first, second)
        |> Interval.midpoint

    let firstWidth =
        Interval.fromEndpoints (first, midpoint)
        |> Interval.width

    let secondWidth =
        Interval.fromEndpoints (second, midpoint)
        |> Interval.width

    Test.equal firstWidth secondWidth


[<Property>]
let ``Singleton creates a zero width interval`` (quantity: Quantity<Unitless>) =
    let interval = Interval.singleton quantity

    Test.all [
        Interval.width interval
        |> Test.equal Quantity.zero

        Interval.isSingleton interval
    ]


[<Property>]
let union (first: Interval<Unitless>) (second: Interval<Unitless>) (ZeroToOne range: float ZeroToOne) =
    let union = Interval.union first second

    let valueInFirst =
        Interval.interpolate first range

    let valueInSecond =
        Interval.interpolate second range

    Test.all [
        Test.isTrue
            $"Value from the first interval, {valueInFirst.Value} is not in the union"
            (Interval.contains valueInFirst union)

        Test.isTrue
            $"Value from the second interval, {valueInSecond.Value} is not in the union"
            (Interval.contains valueInSecond union)
    ]


[<Property>]
let intersection (first: Interval<Unitless>) (second: Interval<Unitless>) (ZeroToOne range: float ZeroToOne) =
    match Interval.intersection first second with
    | Some intersection ->
        let valueInIntersection =
            Interval.interpolate intersection range

        Test.all [
            Test.isTrue
                $"Value from intersection {valueInIntersection} should be in the first interval"
                (Interval.contains valueInIntersection first)

            Test.isTrue
                $"Value from intersection {valueInIntersection} should be in the second interval"
                (Interval.contains valueInIntersection second)
        ]

    | None ->
        Test.isTrue
            "Intervals were found to be intersecting but no intersection was found"
            (not <| Interval.intersects first second)


[<Property>]
let hullN (values: Quantity<Unitless> list) =
    match Interval.hullN values with
    | Some interval ->
        Test.forAll
            (fun value -> $"The value: {value} should be contained with the interval {interval}")
            (fun value -> Interval.contains value interval)
            values

    | None -> Test.equal values []


[<Property>]
let hull3 (a: Quantity<Unitless>) (b: Quantity<Unitless>) (c: Quantity<Unitless>) =
    Test.equal (Interval.hull3 a b c) (Interval.hull a [ b; c ])

[<Property>]
let hullOfN (values: Quantity<Unitless> list) =
    match Interval.hullOfN id values with
    | Some interval ->
        Test.forAll
            (fun value -> $"The value: {value} should be contained with the interval {interval}")
            (fun value -> Interval.contains value interval)
            values

    | None -> Test.equal values []

[<Property>]
let aggregateN (intervals: Interval<Unitless> list) =
    match Interval.aggregateN intervals with
    | Some aggregateInterval ->
        Test.forAll
            (fun interval -> $"The interval {interval} must be contained in the aggregate interval {aggregateInterval}")
            (Interval.isContainedIn aggregateInterval)
            intervals

    | None -> Test.equal [] intervals

[<Property>]
let aggregate3 (a: Interval<Unitless>) (b: Interval<Unitless>) (c: Interval<Unitless>) =
    Test.equal (Interval.aggregate3 a b c) (Interval.aggregate a [ b; c ])

[<Property>]
let aggregateOfN (intervals: Interval<Unitless> list) =
    match Interval.aggregateOfN id intervals with
    | Some aggregateInterval ->
        Test.forAll
            (fun interval -> $"The interval {interval} must be contained in the aggregate interval {aggregateInterval}")
            (Interval.isContainedIn aggregateInterval)
            intervals

    | None -> Test.equal [] intervals

[<Property>]
let ``Intersection and intersects are consistent`` (first: Interval<Unitless>, second: Interval<Unitless>) =
    let intersects =
        Interval.intersects first second

    let maybeIntersection =
        Interval.intersection first second

    Test.equal (maybeIntersection <> None) intersects


// ---- Operation helper functions ----

let testQuantityOperation<'Units, 'ResultUnits>
    (quantityFunction: Quantity<'Units> -> Quantity<'Units> -> Quantity<'ResultUnits>)
    (intervalFunction: Quantity<'Units> -> Interval<'Units> -> Interval<'ResultUnits>)
    : Property =
    let toTuple3 x y z = (x, y, z)

    let arb: Arbitrary<Quantity<'Units> * Interval<'Units> * float> =
        Gen.map3 toTuple3 Gen.quantity Gen.interval (Gen.floatBetween 0. 1.)
        |> Arb.fromGen

    Prop.forAll arb (fun (quantity, interval, t) ->
        let valueInInterval =
            Interval.interpolate interval t

        let quantityResult =
            quantityFunction quantity valueInInterval

        Interval.contains quantityResult (intervalFunction quantity interval))

let testScalarOperation<'Units, 'ResultUnits>
    (scalarGen: Gen<float>)
    (scalarFunction: float -> Quantity<'Units> -> Quantity<'ResultUnits>)
    (intervalFunction: float -> Interval<'Units> -> Interval<'ResultUnits>)
    : Property =
    let toTuple3 x y z = (x, y, z)

    let arb: Arbitrary<float * Interval<'Units> * float> =
        Gen.map3 toTuple3 scalarGen Gen.interval (Gen.floatBetween 0. 1.)
        |> Arb.fromGen

    Prop.forAll arb (fun (scalar, interval, t) ->
        let valueInInterval =
            Interval.interpolate interval t

        let quantityResult =
            scalarFunction scalar valueInInterval

        Interval.contains quantityResult (intervalFunction scalar interval))

let testBinaryOperation<'Units, 'ResultUnits>
    (quantityFunction: Quantity<'Units> -> Quantity<'Units> -> Quantity<'ResultUnits>)
    (intervalFunction: Interval<'Units> -> Interval<'Units> -> Interval<'ResultUnits>)
    : Property =
    let toTuple4 w x y z = (w, x, y, z)
    let zeroToOne = (Gen.floatBetween 0. 1.)

    let arb: Arbitrary<Interval<'Units> * float * Interval<'Units> * float> =
        Gen.map4 toTuple4 Gen.interval zeroToOne Gen.interval zeroToOne
        |> Arb.fromGen

    Prop.forAll arb (fun (firstInterval, t1, secondInterval, t2) ->
        let valueInFirstInterval =
            Interval.interpolate firstInterval t1

        let valueInSecondInterval =
            Interval.interpolate secondInterval t2

        let quantityResult =
            quantityFunction valueInFirstInterval valueInSecondInterval

        Interval.contains quantityResult (intervalFunction firstInterval secondInterval))

let testUnaryOperation<'ResultUnits>
    (quantityFunction: Quantity<Unitless> -> Quantity<'ResultUnits>)
    (intervalFunction: Interval<Unitless> -> Interval<'ResultUnits>)
    : Property =
    let toTuple2 x y = (x, y)
    let zeroToOne = (Gen.floatBetween 0. 1.)

    let arb: Arbitrary<Interval<Unitless> * float> =
        Gen.map2 toTuple2 Gen.interval zeroToOne
        |> Arb.fromGen

    Prop.forAll arb (fun (interval, t) ->
        let value = Interval.interpolate interval t

        let quantityResult = quantityFunction value

        Interval.contains quantityResult (intervalFunction interval))



// ---- Operation Tests ----

[<Property>]
let Plus () =
    testQuantityOperation Quantity.plus Interval.plus

[<Property>]
let Minus () =
    testQuantityOperation Quantity.minus Interval.minus

[<Property>]
let Difference () =
    testQuantityOperation (fun a b -> a |> Quantity.minus b) Interval.difference


[<Property>]
let ``Multiply By`` () =
    testScalarOperation (Gen.floatBetween -10. 10.) Quantity.multiplyBy Interval.multiplyBy

[<Property>]
let ``Divide By`` () =
    let nonZeroGen =
        Gen.oneof [
            (Gen.floatBetween -10 -0.1)
            (Gen.floatBetween 0.1 10)
        ]

    testScalarOperation nonZeroGen Quantity.divideBy Interval.divideBy

[<Property>]
let ``Plus Interval`` () =
    testBinaryOperation Quantity.plus Interval.plusInterval

[<Property>]
let ``Minus Interval`` () =
    testBinaryOperation Quantity.minus Interval.minusInterval

[<Property>]
let Times () =
    testQuantityOperation Quantity.times Interval.times

[<Property>]
let ``Times Unitless`` () =
    testQuantityOperation Quantity.timesUnitless Interval.timesUnitless

[<Property>]
let Product () =
    testQuantityOperation Quantity.times Interval.product

[<Property>]
let ``Times Interval`` () =
    testBinaryOperation Quantity.times Interval.timesInterval

[<Property>]
let ``Times Unitless Interval`` () =
    testBinaryOperation Quantity.timesUnitless Interval.timesUnitlessInterval

[<Property>]
let Abs () =
    testUnaryOperation Quantity.abs Interval.abs

[<Property>]
let Squared () =
    testUnaryOperation Quantity.squared Interval.squared

[<Property>]
let ``Squared Unitless`` () =
    testUnaryOperation Quantity.squaredUnitless Interval.squaredUnitless

[<Property>]
let Cubed () =
    testUnaryOperation Quantity.cubed Interval.cubed

[<Property>]
let ``Cubed Unitless`` () =
    testUnaryOperation Quantity.cubedUnitless Interval.cubedUnitless
