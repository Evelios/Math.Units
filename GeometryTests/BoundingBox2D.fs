module GeometryTests.BoundingBox2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


[<Property>]
let ``Bounding box union contains both boxes``
    (first: BoundingBox2D<Meters, TestSpace>)
    (second: BoundingBox2D<Meters, TestSpace>)
    =
    let union = BoundingBox2D.union first second

    BoundingBox2D.containsBoundingBox first union
    && BoundingBox2D.containsBoundingBox second union
