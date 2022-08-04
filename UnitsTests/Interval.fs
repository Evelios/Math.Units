module UnitsTests.Interval

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Property>]
let hullN (values: Quantity<Meters> list) =
    match Interval.hullN values with
    | Some interval ->
        Test.forAll
            (fun value -> $"The value: {value} should be contained with the interval {interval}")
            (fun value -> Interval.contains value interval)
            values

    | None -> Test.equal values []
