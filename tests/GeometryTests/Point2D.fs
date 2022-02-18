module GeometryTests.Point2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry
open Utilities

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


let ``Point equality test cases`` =
    [ (Point2D.meters 0. 0., Point2D.meters 0. 0.)
      (Point2D.meters -1. -1., Point2D.meters -1. -1.)
      (Point2D.meters 5. 5., Point2D.meters 5. 5.)
      (Point2D.meters 1. 1., Point2D.meters (1. + Epsilon / 2.) (1. + Epsilon / 2.)) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Point equality test cases``)>]
let ``Points are equal`` (lhs: Point2D<Meters, 'Coordiantes>) (rhs: Point2D<Meters, 'Coordinates>) =
    Assert.AreEqual(lhs, rhs)

[<Test>]
let ``Point from polar`` () =
    let expected = Point2D.meters 0. 1.
    let actual = Point2D.ofPolar (Length.meters 1.) (Angle.pi / 2.)

    Assert.AreEqual(expected, actual)


let ``Point less than test cases`` =
    [ (Point2D.meters 0. 0., Point2D.meters 1. 1.)
      (Point2D.meters 0. 0., Point2D.meters 0. 1.) ]
    |> List.map TestCaseData

[<TestCaseSource(nameof ``Point less than test cases``)>]
let ``Point less than`` (lhs: Point2D<Meters, 'Coordinates>) (rhs: Point2D<Meters, 'Coordinates>) = Assert.Less(lhs, rhs)

[<Property>]
let ``Equality and hash code comparison with random points``
    (first: Point2D<Meters, TestSpace>)
    (second: Point2D<Meters, TestSpace>)
    =
    (first = second) = (first.GetHashCode() = second.GetHashCode())
