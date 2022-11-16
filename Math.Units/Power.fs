/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Power</c> value represents power in watts or horsepower. It is stored as a
/// number of watts.
///</summary>
/// 
/// <note>
/// Since <c>Watts</c> are defined as <c>Rate Joules Seconds</c> (energy per unit
/// time), you can construct a <c>Power</c> value using <c>Quantity.per</c>:
/// <code>
///     let power =
///         energy |&gt; Quantity.per duration
/// </code>
/// You can also do rate-related calculations with <c>Power</c> values to compute
/// <c>Energy</c> or <c>Duration</c>:
/// <code>
///     let energy =
///         power |&gt; Quantity.for duration
///     let alsoEnergy =
///         duration |&gt; Quantity.at power
///     let duration =
///         energy |&gt; Quantity.at_ power
/// </code>
/// </note>
///
/// <remark>
/// <a href="https://en.wikipedia.org/wiki/Horsepower#Definitions">Horsepower</a>:
/// Who knew that there were not one, not two, but _three_ possible interpretations
/// of "one horsepower"? (Actually there are more than that, but these three
/// seemed the most reasonable.)
/// </remark>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Power

/// <summary>
/// Construct a <c>Power</c> value from a number of watts.
/// </summary>
let watts (numWatts: float) : Power = Quantity numWatts

/// <summary>
/// Convert a <c>Power</c> value to a number of watts.
/// </summary>
let inWatts (numWatts: Power) : float = numWatts.Value

/// <summary>
/// Construct a <c>Power</c> value from a number of kilowatts.
/// </summary>
let kilowatts (numKilowatts: float) : Power = watts (1000. * numKilowatts)

/// <summary>
/// Convert a <c>Power</c> value to a number of kilowatts.
/// </summary>
let inKilowatts (power: Power) : float = inWatts power / 1000.

/// <summary>
/// Construct a <c>Power</c> value from a number of megawatts.
/// </summary>
let megawatts (numMegawatts: float) : Power = watts (1.0e6 * numMegawatts)

/// <summary>
/// Convert a <c>Power</c> value to a number of megawatts.
/// </summary>
let inMegawatts (power: Power) : float = inWatts power / 1.0e6

/// <summary>
/// Construct a <c>Power</c> value from an number of
/// <a href="https://en.wikipedia.org/wiki/Horsepower#Metric_horsepower">metric horsepower</a>.
/// <code>
///     Power.metricHorsepower 1
///     --&gt; Power.watts 735.49875
/// </code>
/// </summary>
let metricHorsepower (numMetricHorsepower: float) : Power =
    watts (Constants.metricHorsepower * numMetricHorsepower)

/// <summary>
/// Convert a <c>Power</c> value to a number of metric horsepower.
/// </summary>
let inMetricHorsepower (power: Power) : float =
    inWatts power / Constants.metricHorsepower

/// <summary>
/// Construct a <c>Power</c> value from an number of
/// <a href="https://en.wikipedia.org/wiki/Horsepower#Mechanical_horsepower">mechanical horsepower</a>.
/// <code>
///     Power.mechanicalHorsepower 1
///     --&gt; Power.watts 745.6998715822702
/// </code>
/// </summary>
let mechanicalHorsepower (numMechanicalHorsepower: float) : Power =
    watts (
        numMechanicalHorsepower
        * Constants.mechanicalHorsepower
    )

/// <summary>
/// Convert a <c>Power</c> value to a number of mechanical horsepower.
/// </summary>
let inMechanicalHorsepower (power: Power) : float =
    inWatts power / Constants.mechanicalHorsepower

/// <summary>
/// Construct a <c>Power</c> value from an number of
/// <a href="https://en.wikipedia.org/wiki/Horsepower#Electrical_horsepower">electrical horsepower</a>.
/// <code>
///     Power.electricalHorsepower 1
///     --&gt; Power.watts 746
/// </code>
/// </summary>
let electricalHorsepower (numElectricalHorsepower: float) : Power =
    watts (
        Constants.electricalHorsepower
        * numElectricalHorsepower
    )

/// <summary>
/// Convert a <c>Power</c> value to a number of electrical horsepower.
/// </summary>
let inElectricalHorsepower (power: Power) : float =
    inWatts power / Constants.electricalHorsepower
