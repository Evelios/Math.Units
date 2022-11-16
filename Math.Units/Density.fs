/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Density

/// Construct a density from a number of kilograms per cubic meter.
let kilogramsPerCubicMeter (numKilogramsPerCubicMeter: float) : Density = Quantity numKilogramsPerCubicMeter


/// Convert a density to a number of kilograms per cubic meter.
let inKilogramsPerCubicMeter (numKilogramsPerCubicMeter: Density) : float = numKilogramsPerCubicMeter.Value

/// Construct a density from a number of grams per cubic centimeter.
let gramsPerCubicCentimeter (numGramsPerCubicCentimeter: float) : Density =
    kilogramsPerCubicMeter (1000. * numGramsPerCubicCentimeter)


/// Convert a density to a number of grams per cubic centimeter.
let inGramsPerCubicCentimeter (density: Density) : float =
    inKilogramsPerCubicMeter density / 1000.

/// Construct a density from a number of pounds per cubic inch.
let poundsPerCubicInch (numPoundsPerCubicInch: float) : Density =
    kilogramsPerCubicMeter (
        Constants.pound / Constants.cubicInch
        * numPoundsPerCubicInch
    )

/// Convert a density to a number of pounds per cubic inch.
let inPoundsPerCubicInch (density: Density) : float =
    inKilogramsPerCubicMeter density
    / (Constants.pound / Constants.cubicInch)

/// Construct a density from a number of pounds per cubic foot.
let poundsPerCubicFoot (numPoundsPerCubicFoot: float) : Density =
    kilogramsPerCubicMeter (
        Constants.pound / Constants.cubicFoot
        * numPoundsPerCubicFoot
    )

/// Convert a density to a number of pounds per cubic foot.
let inPoundsPerCubicFoot (density: Density) : float =
    inKilogramsPerCubicMeter density
    / (Constants.pound / Constants.cubicFoot)
