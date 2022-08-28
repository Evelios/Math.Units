/// A `Power` value represents power in watts or horsepower. It is stored as a
/// number of watts.
/// Note that since `Watts` is defined as `Rate Joules Seconds` (energy per unit
/// time), you can construct a `Power` value using `Quantity.per`:
///     power =
///         energy |> Quantity.per duration
/// You can also do rate-related calculations with `Power` values to compute
/// `Energy` or `Duration`:
///     energy =
///         power |> Quantity.for duration
///     alsoEnergy =
///         duration |> Quantity.at power
///     duration =
///         energy |> Quantity.at_ power
/// [1]: https://en.wikipedia.org/wiki/Horsepower#Definitions
///
/// ## Horsepower
/// Who knew that there were not one, not two, but _three_ possible interpretations
/// of "one horsepower"? (Actually there are more than that, but these three
/// seemed the most reasonable.)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Power

/// Construct a `Power` value from a number of watts.
let watts (numWatts: float) : Power = Quantity numWatts

/// Convert a `Power` value to a number of watts.
let inWatts (numWatts: Power) : float = numWatts.Value

/// Construct a `Power` value from a number of kilowatts.
let kilowatts (numKilowatts: float) : Power = watts (1000. * numKilowatts)

/// Convert a `Power` value to a number of kilowatts.
let inKilowatts (power: Power) : float = inWatts power / 1000.

/// Construct a `Power` value from a number of megawatts.
let megawatts (numMegawatts: float) : Power = watts (1.0e6 * numMegawatts)

/// Convert a `Power` value to a number of megawatts.
let inMegawatts (power: Power) : float = inWatts power / 1.0e6

/// Construct a `Power` value from an number of [metric horsepower][1].
///     Power.metricHorsepower 1
///     --> Power.watts 735.49875
/// [1]: https://en.wikipedia.org/wiki/Horsepower#Metric_horsepower
let metricHorsepower (numMetricHorsepower: float) : Power =
    watts (Constants.metricHorsepower * numMetricHorsepower)

/// Convert a `Power` value to a number of metric horsepower.
let inMetricHorsepower (power: Power) : float =
    inWatts power / Constants.metricHorsepower

/// Construct a `Power` value from an number of [mechanical horsepower][1].
///     Power.mechanicalHorsepower 1
///     --> Power.watts 745.6998715822702
/// [1]: https://en.wikipedia.org/wiki/Horsepower#Mechanical_horsepower
let mechanicalHorsepower (numMechanicalHorsepower: float) : Power =
    watts (
        numMechanicalHorsepower
        * Constants.mechanicalHorsepower
    )

/// Convert a `Power` value to a number of mechanical horsepower.
let inMechanicalHorsepower (power: Power) : float =
    inWatts power / Constants.mechanicalHorsepower

/// Construct a `Power` value from an number of [electrical horsepower][1].
///     Power.electricalHorsepower 1
///     --> Power.watts 746
/// [1]: https://en.wikipedia.org/wiki/Horsepower#Electrical_horsepower
let electricalHorsepower (numElectricalHorsepower: float) : Power =
    watts (
        Constants.electricalHorsepower
        * numElectricalHorsepower
    )

/// Convert a `Power` value to a number of electrical horsepower.
let inElectricalHorsepower (power: Power) : float =
    inWatts power / Constants.electricalHorsepower
