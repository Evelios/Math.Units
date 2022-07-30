module UnitsTests.Interval

open NUnit.Framework
open FsCheck.NUnit
open FsCheck


[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()
