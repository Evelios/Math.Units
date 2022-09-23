module Math.Units.Tests.Length

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Math.Units
open Math.Units.Test

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

// ---- Order Operators ----

[<Test>]
let ``Length equals`` () =
    Assert.AreEqual(Length.unitless 1., Length.unitless 1.)

[<Test>]
let ``Length less than`` () =
    Assert.Less(Length.unitless 1., Length.unitless 2.)

[<Test>]
let ``Length less than or equal to`` () =
    Assert.LessOrEqual(Length.unitless 1., Length.unitless 1.)
    Assert.LessOrEqual(Length.unitless 1., Length.unitless 2.)

[<Test>]
let ``Length greater than`` () =
    Assert.Greater(Length.unitless 2., Length.unitless 1.)

[<Test>]
let ``Length greater than or equal to`` () =
    Assert.GreaterOrEqual(Length.unitless 2., Length.unitless 2.)
    Assert.GreaterOrEqual(Length.unitless 2., Length.unitless 1.)


[<Property>]
let ``Equality and hash code comparison with random lengths`` (first: Length) (second: Length) =
    (first = second) = (first.GetHashCode() = second.GetHashCode())

// ---- Builders ----

[<Test>]
let ``Length in units equals zero`` () =
    let zero: Quantity<Unitless> = Quantity.zero
    Assert.AreEqual(Length.unitless 0., zero)


// ---- Length Correlations -----

[<Property>]
let ``Conversion to Length and back`` (length: float) =
    let testCases =
        [ Length.meters, Length.inMeters
          Length.microns, Length.inMicrons
          Length.millimeters, Length.inMillimeters
          Length.thou, Length.inThou
          Length.inches, Length.inInches
          Length.centimeters, Length.inCentimeters
          Length.feet, Length.inFeet
          Length.yards, Length.inYards
          Length.kilometers, Length.inKilometers
          Length.miles, Length.inMiles
          Length.astronomicalUnits, Length.inAstronomicalUnits
          Length.parsecs, Length.inParsecs
          Length.lightYears, Length.inLightYears
          Length.angstroms, Length.inAngstroms
          Length.nanometers, Length.inNanometers
          Length.cssPixels, Length.inCssPixels
          Length.points, Length.inPoints
          Length.picas, Length.inPicas ]

    let conversionTest (toLength, fromLength) =
        fromLength (toLength length) .==. length

    List.map conversionTest testCases |> Test.all


// ---- Math Operators ----

[<Property>]
let ``Absolute value is always greater or equal to zero `` (l: Length) = Length.abs l >= Length.zero


[<Test>]
let ``Length addition`` () =
    Assert.AreEqual(Length.meters 20., Length.meters 10. + Length.meters 10.)

[<Test>]
let ``Length subtraction`` () =
    Assert.AreEqual(Length.meters 10., Length.meters 20. - Length.meters 10.)

[<Test>]
let ``Length negation`` () =
    Assert.AreEqual(Length.meters -10., -Length.meters 10.)

[<Test>]
let ``Length multiplication`` () =
    Assert.AreEqual(Length.meters 100., Length.meters 10. * 10.)
    Assert.AreEqual(Length.meters 100., 10. * Length.meters 10.)

[<Test>]
let ``Length division scaling`` () =
    Assert.AreEqual(Length.meters 10., Length.meters 100. / 10.)

[<Test>]
let ``Length division to ratio`` () =
    Assert.AreEqual(10., Length.meters 100. / Length.meters 10.)
