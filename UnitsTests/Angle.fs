module UnitsTests.Angle

open NUnit.Framework
open FsCheck.NUnit
open FsCheck
open System

open Units

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()


// ---- Builders and Accessors

[<Property>]
let ``From radians and back`` (angle: Angle) =
    angle .=. (Angle.inRadians angle |> Angle.radians)

[<Property>]
let ``From degrees and back`` (angle: Angle) =
    angle .=. (Angle.inDegrees angle |> Angle.degrees)

[<Property>]
let ``Normalized value always within -Pi to Pi`` (angle: Angle) =
    Test.all [
        angle
        |> Angle.normalize
        |> Angle.inRadians
        |> Test.greaterThanOrEqualTo -Math.PI

        angle
        |> Angle.normalize
        |> Angle.inRadians
        |> Test.lessThanOrEqualTo Math.PI
    ]


// ---- Order Operators ----

[<Test>]
let ``Angle equality`` () =
    Assert.AreEqual(Angle.degrees 10., Angle.degrees 10.)

[<Test>]
let ``Angle negative equality`` () =
    let lhs =
        Angle.degrees 350. |> Angle.normalize

    let rhs =
        Angle.degrees -10. |> Angle.normalize
    
    Assert.AreEqual(lhs, rhs)

[<Test>]
let ``Angle modular equality`` () =
    let lhs =
        Angle.degrees (350. + 360.) |> Angle.normalize

    let rhs =
        Angle.degrees -10. |> Angle.normalize

    Assert.AreEqual(lhs, rhs)

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

[<Test>]
let ``Angle minimum`` () =
    let smaller = Angle.degrees 50.
    let bigger = Angle.degrees 290.
    let actual = min smaller bigger
    let expected = smaller
    Assert.AreEqual(expected, actual)

[<Test>]
let ``Angle maximum`` () =
    let smaller = Angle.degrees 50.
    let bigger = Angle.degrees 290.
    let actual = max smaller bigger
    let expected = bigger
    Assert.AreEqual(expected, actual)


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

// ---- Modifiers ----

[<Property>]
let ``Half of Twice is identity `` (angle: Angle) =
    Test.equal angle (Angle.twice angle |> Angle.half)
