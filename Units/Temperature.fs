/// Unlike other modules in `elm-units`, this module contains two different
/// primary types:
///   - `Temperature`, which is not actually a `Quantity` since temperatures don't
///     really act like normal quantities. For example, it doesn't make sense to
///     add two temperatures or find the ratio between them.
///   - `Delta`, which represents the difference between two temperatures. A `Delta`
///     _is_ a `Quantity` since it does make sense to add two deltas to get a net
///     delta, find the ratio between two deltas (one rise in temperature might be
///     twice as much as another rise in temperature), etc.
/// Since a `Temperature` value is not a `Quantity`, this module exposes specialized
/// functions for doing the operations on `Temperature` values that _do_ make sense,
/// such as comparing two temperatures or sorting a list of temperatures. It's also
/// possible to find the delta from one temperature to another using [`minus`](Temperature#minus),
/// and then add a `Delta` to a `Temperature` using [`plus`](Temperature#plus).
///
/// @docs Temperature, Delta, CelsiusDegrees
///
/// # Temperatures
///
/// @docs degreesCelsius, inDegreesCelsius, degreesFahrenheit, inDegreesFahrenheit, kelvins, inKelvins, absoluteZero
///
/// # Deltas
///
/// Following the suggestion mentioned [here](https://en.wikipedia.org/wiki/Celsius#Temperatures_and_intervals),
/// this module uses (for example) `celsiusDegrees` to indicate a temperature delta
/// (change in temperature), in contrast to `degreesCelsius` which indicates an
/// actual temperature.
///
/// @docs celsiusDegrees, inCelsiusDegrees, fahrenheitDegrees, inFahrenheitDegrees
///
/// ## Constants
///
/// Shorthand for `Temperature.celsiusDegrees 1` and `Temperature.fahrenheitDegrees
/// 1`. Can be convenient to use with [`Quantity.per`](Quantity#per).
///
/// @docs celsiusDegree, fahrenheitDegree
///
/// # Comparison
///
/// @docs lessThan, greaterThan, lessThanOrEqualTo, greaterThanOrEqualTo, compare, equalWithin, min, max
///
/// # Arithmetic
///
/// @docs plus, minus, clamp
///
/// # List functions
///
/// @docs minimum, maximum, sort, sortBy
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units.Temperature


/// Construct a temperature from a number of [kelvins][kelvin].
///     Temperature.kelvins 300
///     --> Temperature.degreesCelsius 26.85
/// [kelvin]: https://en.wikipedia.org/wiki/Kelvin "Kelvin"
let kelvins (numKelvins: float) : Temperature = Temperature numKelvins

/// Convert a temperature to a number of kelvins.
///    Temperature.degreesCelsius 0
///        |> Temperature.inKelvins
///    --> 273.15
let inKelvins (numKelvins: Temperature) : float = numKelvins.Value

/// Construct a temperature from a number of degrees Celsius.
let degreesCelsius (numDegreesCelsius: float) : Temperature = kelvins (273.15 + numDegreesCelsius)

/// Convert a temperature to a number of degrees Celsius.
let inDegreesCelsius (temperature: Temperature) : float = inKelvins temperature - 273.15

/// Construct a temperature from a number of degrees Fahrenheit.
let degreesFahrenheit (numDegreesFahrenheit: float) : Temperature =
    degreesCelsius ((numDegreesFahrenheit - 32.) / 1.8)

/// Convert a temperature to a number of degrees Fahrenheit.
let inDegreesFahrenheit (temperature: Temperature) : float =
    32. + 1.8 * inDegreesCelsius temperature

/// [Absolute zero](https://en.wikipedia.org/wiki/Absolute_zero), equal to zero
/// kelvins or -273.15 degrees Celsius.
///     Temperature.absoluteZero
///     --> Temperature.degreesCelsius -273.15
let absoluteZero = kelvins 0


// ---- Deltas ----

/// Construct a temperature delta from a number of Celsius degrees.
let celsiusDegrees (numCelsiusDegrees: float) : Delta = Quantity numCelsiusDegrees

/// Convert a temperature delta to a number of Celsius degrees.
let inCelsiusDegrees (numCelsiusDegrees: Delta) : float = numCelsiusDegrees.Value

/// Construct a temperature delta from a number of Fahrenheit degrees.
///    Temperature.fahrenheitDegrees 36
///    --> Temperature.celsiusDegrees 20
let fahrenheitDegrees (numFahrenheitDegrees: float) : Delta =
    celsiusDegrees (numFahrenheitDegrees / 1.8)

/// Convert a temperature delta to a number of Fahrenheit degrees.
///    Temperature.celsiusDegrees 10
///        |> Temperature.inFahrenheitDegrees
///    --> 18
let inFahrenheitDegrees (quantity: Delta) : float = inCelsiusDegrees quantity * 1.8

let celsiusDegree = celsiusDegrees 1
let fahrenheitDegree = fahrenheitDegrees 1


// ---- Functions --------------------------------------------------------------

/// Given a lower and upper bound, clamp a given temperature to within those
/// bounds. Say you wanted to clamp a temperature to be between 18 and 22 degrees
/// Celsius:
///     lowerBound =
///         Temperature.degreesCelsius 18
///     upperBound =
///         Temperature.degreesCelsius 22
///     Temperature.degreesCelsius 25
///         |> Temperature.clamp lowerBound upperBound
///     --> Temperature.degreesCelsius 22
///     Temperature.degreesFahrenheit 67 -- approx 19.4 °C
///         |> Temperature.clamp lowerBound upperBound
///     --> Temperature.degreesFahrenheit 67
///     Temperature.absoluteZero
///         |> Temperature.clamp lowerBound upperBound
///     --> Temperature.degreesCelsius 18
let clamp (lower: Temperature) (upper: Temperature) (temperature: Temperature) : Temperature =
    max lower.Value temperature.Value
    |> min upper.Value
    |> Temperature

// ---- List Functions ---------------------------------------------------------

/// Find the minimum of a list of temperatures. Returns `Nothing` if the list
/// is empty.
///     Temperature.minimum
///         [ Temperature.degreesCelsius 20
///         , Temperature.kelvins 300
///         , Temperature.degreesFahrenheit 74
///         ]
///     --> Just (Temperature.degreesCelsius 20)
let minimum (temperatures: Temperature list) : Temperature option =
    match temperatures with
    | first :: rest -> Some(List.fold min first rest)

    | [] -> None


/// Find the maximum of a list of temperatures. Returns `Nothing` if the list
/// is empty.
///     Temperature.maximum
///         [ Temperature.degreesCelsius 20
///         , Temperature.kelvins 300
///         , Temperature.degreesFahrenheit 74
///         ]
///     --> Just (Temperature.kelvins 300)
let maximum (temperatures: Temperature list) : Temperature option =
    match temperatures with
    | first :: rest -> Some(List.fold max first rest)

    | [] -> None


/// Sort a list of temperatures from lowest to highest.
///     Temperature.sort
///         [ Temperature.degreesCelsius 20
///         , Temperature.kelvins 300
///         , Temperature.degreesFahrenheit 74
///         ]
///     --> [ Temperature.degreesCelsius 20
///     --> , Temperature.degreesFahrenheit 74
///     --> , Temperature.kelvins 300
///     --> ]
let sort (temperatures: Temperature list) : Temperature list = List.sortBy inKelvins temperatures


/// Sort an arbitrary list of values by a derived `Temperature`. If you had
///     rooms =
///         [ ( "Lobby", Temperature.degreesCelsius 21 )
///         , ( "Locker room", Temperature.degreesCelsius 17 )
///         , ( "Rink", Temperature.degreesCelsius -4 )
///         , ( "Sauna", Temperature.degreesCelsius 82 )
///         ]
/// then you could sort by temperature with
///     Temperature.sortBy Tuple.second rooms
///     --> [ ( "Rink", Temperature.degreesCelsius -4 )
///     --> , ( "Locker room", Temperature.degreesCelsius 17 )
///     --> , ( "Lobby", Temperature.degreesCelsius 21 )
///     --> , ( "Sauna", Temperature.degreesCelsius 82 )
///     --> ]
let sortBy (toTemperature: 'a -> Temperature) (list: 'a list) : 'a list =
    let comparator first second =
        compare (toTemperature first) (toTemperature second)

    List.sortWith comparator list
