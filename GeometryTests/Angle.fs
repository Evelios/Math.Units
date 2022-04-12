module GeometryTests.Angle

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Order Operators ----

[<Test>]
let ``Angle equality`` () =
    Assert.AreEqual(Angle.degrees 10., Angle.degrees 10.)
    
[<Test>]
let ``Angle negative equality`` () =
    Assert.AreEqual(Angle.degrees 350., Angle.degrees -10.)
    
[<Test>]
let ``Angle modular equality`` () =
    Assert.AreEqual(Angle.degrees (350. + 360.), Angle.degrees -10.)

[<Test>]
let ``Angle less than`` () =
    Assert.Less(Angle.degrees 0., Angle.degrees 10.)

[<Test>]
let ``Angle less than or equal`` () =
    Assert.LessOrEqual(Angle.degrees 0., Angle.degrees 10.)
    Assert.LessOrEqual(Angle.degrees 10., Angle.degrees 10.)

[<Test>]
let ``Angle greater than`` () =
    Assert.Greater(Angle.degrees 10., Angle.degrees 0.)

[<Test>]
let ``Angle greater than or equal`` () =
    Assert.GreaterOrEqual(Angle.degrees 10., Angle.degrees 0.)
    Assert.GreaterOrEqual(Angle.degrees 10., Angle.degrees 10.)

// ---- Math Operators ----

[<Test>]
let ``Angle addition`` () =
    Assert.AreEqual(Angle.degrees 20., Angle.degrees 10. + Angle.degrees 10.)

[<Test>]
let ``Angle subtraction`` () =
    Assert.AreEqual(Angle.degrees 10., Angle.degrees 20. - Angle.degrees 10.)

[<Test>]
let ``Angle negation`` () =
    Assert.AreEqual(Angle.degrees -10., -Angle.degrees 10.)

[<Test>]
let ``Angle multiplication`` () =
    Assert.AreEqual(Angle.degrees 100., Angle.degrees 10. * 10.)
    Assert.AreEqual(Angle.degrees 100., 10. * Angle.degrees 10.)

[<Test>]
let ``Angle division`` () =
    Assert.AreEqual(Angle.degrees 10., Angle.degrees 100. / 10.)
    
[<Property>]
let ``Equality and hash code comparison with random angles`` (first: Angle) (second: Angle) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())
