/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Density</c> value represents a density in grams per cubic centimeter, pounds
/// per cubic inch, etc. It is stored as a number of kilograms per cubic meter.
/// </summary>
/// 
/// <note>
/// <para>
/// Since <c>KilogramsPerCubicMeter</c> is defined as <c>Rate Kilograms
/// CubicMeters</c> (mass per unit volume), you can construct a <c>Density</c> value using
/// <c>Quantity.per</c>:
/// </para>
/// <code>
///     let density =
///         mass |&gt; Quantity.per volume
/// </code>
/// </note>
///
/// <example>
/// <para>
/// You can also do rate-related calculations with <c>Density</c> values to compute
/// <c>Mass</c> or <c>Volume</c>:
/// </para>
/// <code>
///     let mass =
///         volume |&gt; Quantity.at density
///     let volume =
///         mass |&gt; Quantity.at_ density
/// </code>
/// </example>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Density

/// <category>Metric</category>
/// <summary>Construct a density from a number of kilograms per cubic meter.</summary>
/// <param name="numKilogramsPerCubicMeter">The number of kilograms per cubic meter.</param>
let kilogramsPerCubicMeter (numKilogramsPerCubicMeter: float) : Density = Quantity numKilogramsPerCubicMeter


/// <category>Metric</category>
/// <summary>Convert a density to a number of kilograms per cubic meter.</summary>
/// <param name="density">The density to convert.</param>
let inKilogramsPerCubicMeter (density: Density) : float = density.Value

/// <category>Metric</category>
/// <summary>Construct a density from a number of grams per cubic centimeter.</summary>
/// <param name="numGramsPerCubicCentimeter">The number of grams per cubic centimeter.</param>
let gramsPerCubicCentimeter (numGramsPerCubicCentimeter: float) : Density =
    kilogramsPerCubicMeter (1000. * numGramsPerCubicCentimeter)


/// <category>Metric</category>
/// <summary>Convert a density to a number of grams per cubic centimeter.</summary>
/// <param name="density">The density to convert.</param>
let inGramsPerCubicCentimeter (density: Density) : float =
    inKilogramsPerCubicMeter density / 1000.

/// <category>Imperial</category>
/// <summary>Construct a density from a number of pounds per cubic inch.</summary>
/// <param name="numPoundsPerCubicInch">The number of pounds per cubic inch.</param>
let poundsPerCubicInch (numPoundsPerCubicInch: float) : Density =
    kilogramsPerCubicMeter (
        Constants.pound / Constants.cubicInch
        * numPoundsPerCubicInch
    )

/// <category>Imperial</category>
/// <summary>Convert a density to a number of pounds per cubic inch.</summary>
/// <param name="density">The density to convert.</param>
let inPoundsPerCubicInch (density: Density) : float =
    inKilogramsPerCubicMeter density
    / (Constants.pound / Constants.cubicInch)

/// <category>Imperial</category>
/// <summary>Construct a density from a number of pounds per cubic foot.</summary>
/// <param name="numPoundsPerCubicFoot">The number of pounds per cubic foot.</param>
let poundsPerCubicFoot (numPoundsPerCubicFoot: float) : Density =
    kilogramsPerCubicMeter (
        Constants.pound / Constants.cubicFoot
        * numPoundsPerCubicFoot
    )

/// <category>Imperial</category>
/// <summary>Convert a density to a number of pounds per cubic foot.</summary>
/// <param name="density">The density to convert.</param>
let inPoundsPerCubicFoot (density: Density) : float =
    inKilogramsPerCubicMeter density
    / (Constants.pound / Constants.cubicFoot)
