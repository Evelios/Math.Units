module GeometryTests.Triangle2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Property>]
let ``non-zero area triangle contains its own centroid`` (triangle: Triangle2D<Meters, TestSpace>) =
    let centroid = Triangle2D.centroid triangle
    let area = Triangle2D.area triangle

    Test.isTrue
        "non-zero area triangle did not contain its own centroid"
        (area = Length.zero
         || Triangle2D.contains centroid triangle)
