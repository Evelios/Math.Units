module GeometryTests.Rectangle2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()
