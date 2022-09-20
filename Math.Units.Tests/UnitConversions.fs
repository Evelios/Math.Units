module Math.Units.Tests.UnitConversions

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Math.Units
open Math.Units.Test

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

let equalityTest (title: string) (unit: string) (first: 'a, second: 'a) : unit =
    Assert.AreEqual(first, second, $"{title}: {first}{unit} and {second}{unit} should be equal.")

let equalPairs (title: string) (unit: string) (pairs: ('a * 'a) list) =
    let unitTest = equalityTest title unit

    List.iter unitTest pairs

let conversionTests (num: float) (pairs: ((float -> 'a) * ('a -> float)) list) : bool =
    List.forall
        (fun (intoQuantity, fromQuantity) ->
            let quantity = intoQuantity num
            let converted = fromQuantity quantity
            Test.almostEqual num converted)
        pairs

[<Property>]
let ``Angle Conversions`` (angle: float) =
    conversionTests
        angle
        [ Angle.degrees, Angle.inDegrees
          Angle.radians, Angle.inRadians
          Angle.turns, Angle.inTurns
          Angle.minutes, Angle.inMinutes
          Angle.seconds, Angle.inSeconds ]

[<Property>]
let ``Temperature Conversions`` (temp: float) =
    conversionTests
        temp
        [ Temperature.kelvins, Temperature.inKelvins
          Temperature.degreesCelsius, Temperature.inDegreesCelsius
          Temperature.degreesFahrenheit, Temperature.inDegreesFahrenheit ]

[<Property>]
let ``Temperature Delta Conversions`` (temp: float) =
    conversionTests
        temp
        [ Temperature.celsiusDegrees, Temperature.inCelsiusDegrees
          Temperature.fahrenheitDegrees, Temperature.inFahrenheitDegrees ]

[<Test>]
let Lengths () =
    equalPairs
        "Lengths"
        "m"
        [ Length.inches 1., Length.centimeters 2.54
          Length.feet 3., Length.yards 1.
          Length.miles 1., Length.feet 5280
          Length.meters 1., Length.microns 1.e6
          Length.angstroms 2.e10, Length.meters 2.
          Length.nanometers 1., Length.angstroms 10.
          Length.cssPixels 1., Length.inches (1. / 96.)
          Length.points 1., Length.inches (1. / 72.)
          Length.picas 1., Length.inches (1. / 6.) ]


[<Property>]
let ``Length Conversions`` (length: float) =
    conversionTests
        length
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


[<Property>]
let ``Area Conversions`` (area: float) =
    conversionTests
        area
        [ Area.squareMeters, Area.inSquareMeters
          Area.squareMillimeters, Area.inSquareMillimeters
          Area.squareInches, Area.inSquareInches
          Area.squareCentimeters, Area.inSquareCentimeters
          Area.squareKilometers, Area.inSquareKilometers
          Area.squareFeet, Area.inSquareFeet
          Area.squareYards, Area.inSquareYards
          Area.hectares, Area.inHectares
          Area.acres, Area.inAcres
          Area.squareMiles, Area.inSquareMiles ]


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


[<Property>]
let ``Volume Conversions`` (volume: float) =
    conversionTests
        volume
        [ Volume.cubicMeters, Volume.inCubicMeters
          Volume.cubicInches, Volume.inCubicInches
          Volume.cubicFeet, Volume.inCubicFeet
          Volume.cubicYards, Volume.inCubicYards
          Volume.milliliters, Volume.inMilliliters
          Volume.cubicCentimeters, Volume.inCubicCentimeters
          Volume.liters, Volume.inLiters
          Volume.usLiquidGallons, Volume.inUsLiquidGallons
          Volume.usDryGallons, Volume.inUsDryGallons
          Volume.imperialGallons, Volume.inImperialGallons
          Volume.usLiquidQuarts, Volume.inUsLiquidQuarts
          Volume.usDryQuarts, Volume.inUsDryQuarts
          Volume.imperialQuarts, Volume.inImperialQuarts
          Volume.usLiquidPints, Volume.inUsLiquidPints
          Volume.usDryPints, Volume.inUsDryPints
          Volume.imperialPints, Volume.inImperialPints
          Volume.usFluidOunces, Volume.inUsFluidOunces
          Volume.imperialFluidOunces, Volume.inImperialFluidOunces ]


[<Test>]
let Speeds () =
    equalPairs
        "Speeds"
        "m/s"
        [ (Speed.metersPerSecond 2.5,
           Length.meters 5.
           |> Quantity.per (Duration.seconds 2.))
          (Speed.milesPerHour 60.,
           Length.miles 1.
           |> Quantity.per (Duration.minutes 1.))
          (Speed.metersPerSecond 299792458.,
           Length.lightYears 1
           |> Quantity.per (Duration.julianYears 1.)) ]

[<Property>]
let ``Speed Conversions`` (speed: float) =
    conversionTests
        speed
        [ Speed.metersPerSecond, Speed.inMetersPerSecond
          Speed.feetPerSecond, Speed.inFeetPerSecond
          Speed.kilometersPerHour, Speed.inKilometersPerHour
          Speed.milesPerHour, Speed.inMilesPerHour ]


[<Property>]
let ``Acceleration Conversions`` (acceleration: float) =
    conversionTests
        acceleration
        [ Acceleration.metersPerSecondSquared, Acceleration.inMetersPerSecondSquared
          Acceleration.feetPerSecondSquared, Acceleration.inFeetPerSecondSquared
          Acceleration.gees, Acceleration.inGees ]


[<Test>]
let ``Angular Speeds`` () =
    equalPairs "Angular Speeds" "rad/s" [ AngularSpeed.turnsPerSecond 1., AngularSpeed.turnsPerMinute 60. ]

[<Property>]
let ``Angular Speed Conversions`` (angularSpeed: float) =
    conversionTests
        angularSpeed
        [ AngularSpeed.degreesPerSecond, AngularSpeed.inDegreesPerSecond
          AngularSpeed.radiansPerSecond, AngularSpeed.inRadiansPerSecond
          AngularSpeed.turnsPerSecond, AngularSpeed.inTurnsPerSecond
          AngularSpeed.turnsPerMinute, AngularSpeed.inTurnsPerMinute
          AngularSpeed.revolutionsPerSecond, AngularSpeed.inTurnsPerSecond
          AngularSpeed.revolutionsPerMinute, AngularSpeed.inTurnsPerMinute ]


[<Test>]
let ``Angular Accelerations`` () =
    equalPairs
        "Angular Accelerations"
        "rad/s"
        [ (AngularAcceleration.degreesPerSecondSquared 360., AngularAcceleration.turnsPerSecondSquared 1.) ]

[<Property>]
let ``Angular Acceleration Conversions`` (angularAcceleration: float) =
    conversionTests
        angularAcceleration
        [ AngularAcceleration.degreesPerSecondSquared, AngularAcceleration.inDegreesPerSecondSquared
          AngularAcceleration.radiansPerSecondSquared, AngularAcceleration.inRadiansPerSecondSquared
          AngularAcceleration.turnsPerSecondSquared, AngularAcceleration.inTurnsPerSecondSquared

          ]

[<Test>]
let Powers () =
    equalPairs
        "Powers"
        "W"
        [ (Power.watts 50.,
           Current.amperes 5.
           |> Quantity.at (
               Current.amperes 5.
               |> Quantity.at (Resistance.ohms 2.)
           )) ]

[<Property>]
let ``Power Conversions`` (power: float) =
    conversionTests
        power
        [ Power.watts, Power.inWatts
          Power.kilowatts, Power.inKilowatts
          Power.megawatts, Power.inMegawatts
          Power.metricHorsepower, Power.inMetricHorsepower
          Power.mechanicalHorsepower, Power.inMechanicalHorsepower
          Power.electricalHorsepower, Power.inElectricalHorsepower ]


[<Test>]
let Pressures () =
    equalPairs "Pressures" "Pa" [ (Pressure.atmospheres 1., Pressure.kilopascals 101.325) ]

[<Property>]
let ``Pressures Conversions`` (pressure: float) =
    conversionTests
        pressure
        [ Pressure.pascals, Pressure.inPascals
          Pressure.kilopascals, Pressure.inKilopascals
          Pressure.megapascals, Pressure.inMegapascals
          Pressure.poundsPerSquareInch, Pressure.inPoundsPerSquareInch
          Pressure.atmospheres, Pressure.inAtmospheres ]


[<Test>]
let Masses () =
    equalPairs
        "Masses"
        "kg"
        [ (Mass.grams 1., Mass.kilograms 0.001)
          (Mass.ounces 1., Mass.pounds 0.0625)
          (Mass.pounds 1., Mass.kilograms 0.45359237) ]


[<Property>]
let ``Mass Conversions`` (mass: float) =
    conversionTests
        mass
        [ Mass.kilograms, Mass.inKilograms
          Mass.grams, Mass.inGrams
          Mass.pounds, Mass.inPounds
          Mass.ounces, Mass.inOunces
          Mass.metricTons, Mass.inMetricTons
          Mass.shortTons, Mass.inShortTons
          Mass.longTons, Mass.inLongTons ]


[<Test>]
let Densities () =
    equalPairs "Densities" "kg/m^3" [ (Density.gramsPerCubicCentimeter 1., Density.kilogramsPerCubicMeter 1000.) ]


[<Property>]
let ``Densities Conversions`` (density: float) =
    conversionTests
        density
        [ Density.kilogramsPerCubicMeter, Density.inKilogramsPerCubicMeter
          Density.gramsPerCubicCentimeter, Density.inGramsPerCubicCentimeter
          Density.poundsPerCubicInch, Density.inPoundsPerCubicInch
          Density.poundsPerCubicFoot, Density.inPoundsPerCubicFoot ]


[<Test>]
let Durations () =
    equalPairs "Durations" "s" [ (Duration.julianYears 1., Duration.days 365.25) ]


[<Property>]
let ``Duration Conversions`` (duration: float) =
    conversionTests
        duration
        [ Duration.seconds, Duration.inSeconds
          Duration.milliseconds, Duration.inMilliseconds
          Duration.minutes, Duration.inMinutes
          Duration.hours, Duration.inHours
          Duration.days, Duration.inDays
          Duration.weeks, Duration.inWeeks
          Duration.julianYears, Duration.inJulianYears ]


[<Test>]
let ``Substance Amount`` () =
    equalPairs
        "SubstanceAmounts"
        "ν"
        [ (SubstanceAmount.nanomoles 20., SubstanceAmount.picomoles 20000.)
          (SubstanceAmount.nanomoles 7000., SubstanceAmount.micromoles 7.)
          (SubstanceAmount.millimoles 3., SubstanceAmount.micromoles 3000.)
          (SubstanceAmount.nanomoles 1000000., SubstanceAmount.millimoles 1.)
          (SubstanceAmount.centimoles 600., SubstanceAmount.decimoles 60.)
          (SubstanceAmount.moles 1., SubstanceAmount.millimoles 1000.)
          (SubstanceAmount.moles 4., SubstanceAmount.centimoles 400.)
          (SubstanceAmount.moles 2., SubstanceAmount.decimoles 20.)
          (SubstanceAmount.moles 2000., SubstanceAmount.kilomoles 2.)
          (SubstanceAmount.kilomoles 1000., SubstanceAmount.megamoles 1.)
          (SubstanceAmount.megamoles 1000., SubstanceAmount.gigamoles 1.) ]


[<Property>]
let ``Substance Amount Conversions`` (substanceAmount: float) =
    conversionTests
        substanceAmount
        [ SubstanceAmount.moles, SubstanceAmount.inMoles
          SubstanceAmount.picomoles, SubstanceAmount.inPicomoles
          SubstanceAmount.nanomoles, SubstanceAmount.inNanomoles
          SubstanceAmount.micromoles, SubstanceAmount.inMicromoles
          SubstanceAmount.millimoles, SubstanceAmount.inMillimoles
          SubstanceAmount.centimoles, SubstanceAmount.inCentimoles
          SubstanceAmount.kilomoles, SubstanceAmount.inKilomoles
          SubstanceAmount.gigamoles, SubstanceAmount.inGigamoles ]


[<Property>]
let ``Pixel Conversions`` (pixels: float) =
    conversionTests pixels [ Pixels.float, Pixels.toFloat ]


[<Property>]
let ``Pixels Per Second Conversions`` (pixels: float) =
    conversionTests pixels [ Pixels.pixelsPerSecond, Pixels.inPixelsPerSecond ]


[<Property>]
let ``Pixels Per Second Squared Conversions`` (pixels: float) =
    conversionTests pixels [ Pixels.pixelsPerSecondSquared, Pixels.inPixelsPerSecondSquared ]


[<Property>]
let ``Square Pixels Conversions`` (pixels: float) =
    conversionTests pixels [ Pixels.squarePixels, Pixels.inSquarePixels ]


[<Property>]
let ``Solid Angle Conversions`` (solidAngle: float) =
    conversionTests
        solidAngle
        [ SolidAngle.steradians, SolidAngle.inSteradians
          SolidAngle.spats, SolidAngle.inSpats
          SolidAngle.squareDegrees, SolidAngle.inSquareDegrees ]


[<Property>]
let ``Molarity Conversions`` (molarity: float) =
    conversionTests
        molarity
        [ Molarity.molesPerCubicMeter, Molarity.inMolesPerCubicMeter
          Molarity.molesPerLiter, Molarity.inMolesPerLiter
          Molarity.decimolesPerLiter, Molarity.inDecimolesPerLiter
          Molarity.centimolesPerLiter, Molarity.inCentimolesPerLiter
          Molarity.millimolesPerLiter, Molarity.inMillimolesPerLiter
          Molarity.micromolesPerLiter, Molarity.inMicromolesPerLiter ]


[<Property>]
let ``Voltage Conversions`` (voltage: float) =
    conversionTests voltage [ Voltage.volts, Voltage.inVolts ]


[<Property>]
let ``Charge Conversions`` (charge: float) =
    conversionTests
        charge
        [ Charge.coulombs, Charge.inCoulombs
          Charge.ampereHours, Charge.inAmpereHours
          Charge.milliampereHours, Charge.inMilliampereHours ]

let ``Resistance Conversions`` (resistance: float) =
    conversionTests resistance [ Resistance.ohms, Resistance.inOhms ]


[<Property>]
let ``Capacitance Conversions`` (capacitance: float) =
    conversionTests
        capacitance
        [ Capacitance.farads, Capacitance.inFarads
          Capacitance.microfarads, Capacitance.inMicrofarads
          Capacitance.nanofarads, Capacitance.inNanofarads
          Capacitance.picofarads, Capacitance.inPicofarads ]


[<Test>]
let Inductances () =
    equalPairs
        "Inductance"
        "H"
        [ (Inductance.henries 1000, Inductance.kilohenries 1)
          (Inductance.nanohenries 1000, Inductance.microhenries 1)
          (Inductance.kilohenries 10, Inductance.millihenries 10000000) ]


[<Property>]
let ``Inductance Conversions`` (inductance: float) =
    conversionTests
        inductance
        [ Inductance.henries, Inductance.inHenries
          Inductance.millihenries, Inductance.inMillihenries
          Inductance.microhenries, Inductance.inMicrohenries
          Inductance.nanohenries, Inductance.inNanohenries
          Inductance.kilohenries, Inductance.inKilohenries ]


[<Property>]
let ``Luminance Conversions`` (luminance: float) =
    conversionTests
        luminance
        [ Luminance.nits, Luminance.inNits
          Luminance.footLamberts, Luminance.inFootLamberts ]


[<Property>]
let ``Luminous Flux Conversions`` (lumens: float) =
    conversionTests lumens [ LuminousFlux.lumens, LuminousFlux.inLumens ]


[<Property>]
let ``Illuminance Conversions`` (lux: float) =
    conversionTests
        lux
        [ Illuminance.lux, Illuminance.inLux
          Illuminance.footCandles, Illuminance.inFootCandles ]


[<Property>]
let ``Energy Conversions`` (energy: float) =
    conversionTests
        energy
        [ Energy.joules, Energy.inJoules
          Energy.kilojoules, Energy.inKilojoules
          Energy.megajoules, Energy.inMegajoules
          Energy.kilowattHours, Energy.inKilowattHours ]
