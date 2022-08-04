module UnitsTests.Interval

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

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
