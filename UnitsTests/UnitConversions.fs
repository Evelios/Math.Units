module UnitsTests.UnitConversions

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Units

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

let equalityTest (title: string) (unit: string) (first: Quantity<'Unit>, second: Quantity<'Unit>) : unit =
    Assert.AreEqual(first, second, $"{title}: {first}{unit} and {second}{unit} should be equal.")

let equalPairs (title: string) (unit: string) (pairs: (Quantity<'Unit> * Quantity<'Unit>) list) =
    let unitTest = equalityTest title unit

    List.iter unitTest pairs

[<Test>]
let Lengths () =
    equalPairs
        "Lengths"
        "m"
        [ (Length.inches 1., Length.centimeters 2.54)
          (Length.feet 3., Length.yards 1.)
          (Length.miles 1., Length.feet 5280)
          (Length.meters 1., Length.microns 1.e6)
          (Length.angstroms 2.e10, Length.meters 2.)
          (Length.nanometers 1., Length.angstroms 10.)
          (Length.cssPixels 1., Length.inches (1. / 96.))
          (Length.points 1., Length.inches (1. / 72.))
          (Length.picas 1., Length.inches (1. / 6.)) ]

[<Test>]
let Volumes () =
    equalPairs
        "Volumes"
        "m^3"
        [ (Volume.cubicMeters 2., Volume.usLiquidGallons (2. * 264.17205235814845))
          (Volume.imperialGallons 219.96924829908778, Volume.usDryGallons 227.02074606721396)
          (Volume.cubicInches (36. * 36. * 36.), Volume.cubicYards 1.)
          (Volume.usLiquidGallons 1., Volume.usLiquidQuarts 4.)
          (Volume.usDryGallons 1., Volume.usDryQuarts 4.)
          (Volume.imperialGallons 1., Volume.imperialQuarts 4.)
          (Volume.usLiquidQuarts 1., Volume.usLiquidPints 2.)
          (Volume.usDryQuarts 1., Volume.usDryPints 2.)
          (Volume.imperialQuarts 1., Volume.imperialPints 2.)
          (Volume.usLiquidPints 1., Volume.usFluidOunces 16.)
          (Volume.imperialPints 1., Volume.imperialFluidOunces 20.)
          (Volume.cubicCentimeters 1., Volume.milliliters 1.) ]
