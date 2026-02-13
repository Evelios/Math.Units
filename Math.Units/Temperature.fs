/// <category>Module: Unit System</category>
/// <summary>
/// Unlike other modules in <c>Math.Units</c>, this module contains two different
/// primary types:
/// <list type="bullet">
/// <item><description>
///     <c>Temperature</c>, which is not actually a <c>Quantity</c> since temperatures don't
///     really act like normal quantities. For example, it doesn't make sense to
///     add two temperatures or find the ratio between them.
/// </description></item>
/// <item><description>
///     <c>TemperatureDelta</c>, which represents the difference between two temperatures. A <c>TemperatureDelta</c>
///     <i>is</i> a <c>Quantity</c> since it does make sense to add two deltas to get a net
///     delta, find the ratio between two deltas (one rise in temperature might be
///     twice as much as another rise in temperature), etc.
/// </description></item>
/// </list>
/// </summary>
/// 
/// <note>
/// Since a <c>Temperature</c> value is not a <c>Quantity</c>, this module exposes specialized
/// functions for doing the operations on <c>Temperature</c> values that <i>do</i> make sense,
/// such as comparing two temperatures or sorting a list of temperatures. It's also
/// possible to find the delta from one temperature to another using <c>Temperature.minus</c>,
/// and then add a <c>TemperatureDelta</c> to a <c>Temperature</c> using <c>Temperature.plus</c>.
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Temperature

/// <category>Temperature Conversions</category>
/// <summary>
/// Construct a temperature from a number of
/// <a href="https://en.wikipedia.org/wiki/Kelvin">kelvins</a>.
/// </summary>
/// 
/// <example><code>
///     Temperature.kelvins 300
///     --&gt; Temperature.degreesCelsius 26.85
/// </code></example>
/// <param name="numKelvins">The number of kelvins.</param>
let kelvins (numKelvins: float) : Temperature = Temperature numKelvins

/// <category>Temperature Conversions</category>
/// <summary>
/// Convert a temperature to a number of kelvins.
/// </summary>
/// <example><code>
///    Temperature.degreesCelsius 0
///        |&gt; Temperature.inKelvins
///    --&gt; 273.15
/// </code></example>
/// <param name="temperature">The temperature to convert.</param>
let inKelvins (temperature: Temperature) : float = temperature.Value

/// <category>Temperature Conversions</category>
/// <summary>Construct a temperature from a number of degrees Celsius.</summary>
/// <param name="numDegreesCelsius">The number of degrees Celsius.</param>
let degreesCelsius (numDegreesCelsius: float) : Temperature = kelvins (273.15 + numDegreesCelsius)

/// <category>Temperature Conversions</category>
/// <summary>Convert a temperature to a number of degrees Celsius.</summary>
/// <param name="temperature">The temperature to convert.</param>
let inDegreesCelsius (temperature: Temperature) : float = inKelvins temperature - 273.15

/// <category>Temperature Conversions</category>
/// <summary>Construct a temperature from a number of degrees Fahrenheit.</summary>
/// <param name="numDegreesFahrenheit">The number of degrees Fahrenheit.</param>
let degreesFahrenheit (numDegreesFahrenheit: float) : Temperature =
    degreesCelsius ((numDegreesFahrenheit - 32.) / 1.8)

/// <category>Temperature Conversions</category>
/// <summary>Convert a temperature to a number of degrees Fahrenheit.</summary>
/// <param name="temperature">The temperature to convert.</param>
let inDegreesFahrenheit (temperature: Temperature) : float =
    32. + 1.8 * inDegreesCelsius temperature

/// <category>Constants</category>
/// <summary>
/// <a href="https://en.wikipedia.org/wiki/Absolute_zero">Absolute zero</a>,
/// equal to zero kelvins or -273.15 degrees Celsius.
/// </summary>
/// 
/// <example><code>
///     Temperature.absoluteZero
///     --&gt; Temperature.degreesCelsius -273.15
/// </code></example>
let absoluteZero = kelvins 0


// ---- Deltas ----

/// <category>Temperature Delta Conversions</category>
/// <summary>Construct a temperature delta from a number of Celsius degrees.</summary>
/// <param name="numCelsiusDegrees">The number of Celsius degrees.</param>
let celsiusDegrees (numCelsiusDegrees: float) : TemperatureDelta = Quantity numCelsiusDegrees

/// <category>Temperature Delta Conversions</category>
/// <summary>Convert a temperature delta to a number of Celsius degrees.</summary>
/// <param name="temperatureDelta">The temperature delta to convert.</param>
let inCelsiusDegrees (temperatureDelta: TemperatureDelta) : float = temperatureDelta.Value

/// <category>Temperature Delta Conversions</category>
/// <summary>
/// Construct a temperature delta from a number of Fahrenheit degrees.
/// </summary>
/// 
/// <example><code>
///    Temperature.fahrenheitDegrees 36
///    --&gt; Temperature.celsiusDegrees 20
/// </code></example>
/// <param name="numFahrenheitDegrees">The number of Fahrenheit degrees.</param>
let fahrenheitDegrees (numFahrenheitDegrees: float) : TemperatureDelta =
    celsiusDegrees (numFahrenheitDegrees / 1.8)

/// <category>Temperature Delta Conversions</category>
/// <summary>
/// Convert a temperature delta to a number of Fahrenheit degrees.
/// </summary>
/// <example><code>
///    Temperature.celsiusDegrees 10
///        |&gt; Temperature.inFahrenheitDegrees
///    --&gt; 18
/// </code></example>
/// <param name="temperatureDelta">The temperature delta to convert.</param>
let inFahrenheitDegrees (temperatureDelta: TemperatureDelta) : float = inCelsiusDegrees temperatureDelta * 1.8

/// <category>Constants</category>
let celsiusDegree = celsiusDegrees 1
/// <category>Constants</category>
let fahrenheitDegree = fahrenheitDegrees 1

// ---- Operators --------------------------------------------------------------

/// <category>Operators</category>
/// <summary>
/// This is meant to be used with pipe operators.
/// <code>
///     Temperature.inDegreesCelsius 10.
///     |&gt; Temperature.greaterThan (Temperature.inDegreesCelsius 15.)
/// </code>
/// </summary>
/// <param name="rhs">The temperature to compare against.</param>
/// <param name="lhs">The temperature being compared.</param>
let lessThan (rhs: Temperature) (lhs: Temperature) : bool = lhs < rhs

/// <category>Operators</category>
/// <summary>
/// This is meant to be used with pipe operators.
/// <code>
///     Temperature.inDegreesCelsius 10.
///     |&gt; Temperature.greaterThanOrEqualTo (Temperature.inDegreesCelsius 15.)
/// </code>
/// </summary>
/// <param name="rhs">The temperature to compare against.</param>
/// <param name="lhs">The temperature being compared.</param>
let lessThanOrEqualTo (rhs: Temperature) (lhs: Temperature) : bool = lhs <= rhs

/// <category>Operators</category>
/// <summary>
/// This is meant to be used with pipe operators.
/// <code>
///     Temperature.inDegreesCelsius 30.
///     |&gt; Temperature.greaterThan (Temperature.inDegreesCelsius 15.)
/// </code>
/// </summary>
/// <param name="rhs">The temperature to compare against.</param>
/// <param name="lhs">The temperature being compared.</param>
let greaterThan (rhs: Temperature) (lhs: Temperature) : bool = lhs > rhs

/// <category>Operators</category>
/// <summary>
/// This is meant to be used with pipe operators.
/// <code>
///     Temperature.inDegreesCelsius 30.
///     |&gt; Temperature.greaterThan (Temperature.inDegreesCelsius 15.)
/// </code>
/// </summary>
/// <param name="rhs">The temperature to compare against.</param>
/// <param name="lhs">The temperature being compared.</param>
let greaterThanOrEqualTo (rhs: Temperature) (lhs: Temperature) : bool = lhs >= rhs


/// <category>Operators</category>
/// <summary>
/// This is meant to be used with pipe operators.
/// <code>
///     Temperature.inDegreesCelsius 20.
///     |&gt; Temperature.plus (Temperature.celsiusDegrees 35.)
///     --&gt; Temperature.inDegreesCelsius 55.
/// </code>
/// </summary>
/// <param name="rhs">The temperature delta to add.</param>
/// <param name="lhs">The temperature to add to.</param>
let plus (rhs: TemperatureDelta) (lhs: Temperature) : Temperature = lhs + rhs

/// <category>Operators</category>
/// <summary>
/// Find the delta from the first temperature to the second.
/// <code>
///     Temperature.minus
///         (Temperature.degreesCelsius 20)
///         (Temperature.degreesCelsius 25)
///     --&gt; Temperature.celsiusDegrees 5
/// </code>
/// </summary>
/// <param name="lhs">The first temperature.</param>
/// <param name="rhs">The second temperature.</param>
let minus (lhs: Temperature) (rhs: Temperature) : TemperatureDelta = rhs - lhs

/// <category>Operators</category>
/// <summary>
/// Find the ratio between two temperature deltas.
/// </summary>
/// <param name="x">The first temperature delta.</param>
/// <param name="y">The second temperature delta.</param>
let ratio (x: TemperatureDelta) (y: TemperatureDelta) : float = Quantity.ratio x y


// ---- Functions --------------------------------------------------------------

/// <category>Modifiers</category>
/// <summary>
/// Given a lower and upper bound, clamp a given temperature to within those
/// bounds.
/// </summary>
///
/// <example>
/// Say you wanted to clamp a temperature to be between 18 and 22 degrees /// Celsius:
/// <code>
///     let lowerBound =
///         Temperature.degreesCelsius 18
///     let upperBound =
///         Temperature.degreesCelsius 22
/// 
///     let Temperature.degreesCelsius 25
///         |&gt; Temperature.clamp lowerBound upperBound
///     --&gt; Temperature.degreesCelsius 22
/// 
///     let Temperature.degreesFahrenheit 67 -- approx 19.4 °C
///         |&gt; Temperature.clamp lowerBound upperBound
///     --&gt; Temperature.degreesFahrenheit 67
/// 
///     let Temperature.absoluteZero
///         |&gt; Temperature.clamp lowerBound upperBound
///     --&gt; Temperature.degreesCelsius 18
/// </code></example>
/// <param name="lower">The lower bound.</param>
/// <param name="upper">The upper bound.</param>
/// <param name="temperature">The temperature to clamp.</param>
let clamp (lower: Temperature) (upper: Temperature) (temperature: Temperature) : Temperature =
    max lower.Value temperature.Value
    |> min upper.Value
    |> Temperature

/// <category>Modifiers</category>
/// <summary>Round a temperature.</summary>
/// <param name="temp">The temperature to round.</param>
let round (temp: Temperature) : Temperature = round temp

/// <category>Modifiers</category>
/// <summary>Round a temperature down.</summary>
/// <param name="temp">The temperature to round down.</param>
let floor (temp: Temperature) : Temperature = floor temp

/// <category>Modifiers</category>
/// <summary>Round a temperature up.</summary>
/// <param name="temp">The temperature to round up.</param>
let ceil (temp: Temperature) : Temperature = ceil temp

/// <category>Modifiers</category>
/// <summary>Truncate a temperature.</summary>
/// <param name="temp">The temperature to truncate.</param>
let truncate (temp: Temperature) : Temperature = truncate temp

/// <category>Modifiers</category>
/// <summary>Find the minimum of two temperatures.</summary>
/// <param name="first">The first temperature.</param>
/// <param name="second">The second temperature.</param>
let min (first: Temperature) (second: Temperature) : Temperature = min first second

/// <category>Modifiers</category>
/// <summary>Find the maximum of two temperatures.</summary>
/// <param name="first">The first temperature.</param>
/// <param name="second">The second temperature.</param>
let max (first: Temperature) (second: Temperature) : Temperature = max first second

// ---- List Functions ---------------------------------------------------------

/// <category>List Functions</category>
/// <summary>
/// Find the minimum of a list of temperatures. Returns <c>Nothing</c> if the list
/// is empty.
/// </summary>
/// <example><code>
///     Temperature.minimum
///         [ Temperature.degreesCelsius 20
///           Temperature.kelvins 300
///           Temperature.degreesFahrenheit 74
///         ]
///     --&gt; Some (Temperature.degreesCelsius 20)
/// </code></example>
/// <param name="temperatures">The list of temperatures.</param>
let minimum (temperatures: Temperature list) : Temperature option =
    match temperatures with
    | first :: rest -> Some(List.fold min first rest)
    | [] -> None


/// <category>List Functions</category>
/// <summary>
/// Find the maximum of a list of temperatures. Returns <c>Nothing</c> if the list
/// is empty.
/// </summary>
/// <example><code>
///     Temperature.maximum
///         [ Temperature.degreesCelsius 20
///           Temperature.kelvins 300
///           Temperature.degreesFahrenheit 74
///         ]
///     --&gt; Some (Temperature.kelvins 300)
/// </code></example>
/// <param name="temperatures">The list of temperatures.</param>
let maximum (temperatures: Temperature list) : Temperature option =
    match temperatures with
    | first :: rest -> Some(List.fold max first rest)

    | [] -> None


/// <category>List Functions</category>
/// <summary>
/// Sort a list of temperatures from lowest to highest.
/// <code>
///     Temperature.sort
///         [ Temperature.degreesCelsius 20
///           Temperature.kelvins 300
///           Temperature.degreesFahrenheit 74
///         ]
///     --&gt; [ Temperature.degreesCelsius 20
///     --&gt;   Temperature.degreesFahrenheit 74
///     --&gt;   Temperature.kelvins 300
///     --&gt; ]
/// </code>
/// </summary>
/// <param name="temperatures">The list of temperatures.</param>
let sort (temperatures: Temperature list) : Temperature list = List.sortBy inKelvins temperatures


/// <category>List Functions</category>
/// <summary>
/// Sort an arbitrary list of values by a derived <c>Temperature</c>.
/// </summary>
///
/// <example>
/// <para>If you had</para>
/// <code>
///     let rooms =
///         [ ( "Lobby", Temperature.degreesCelsius 21 )
///           ( "Locker room", Temperature.degreesCelsius 17 )
///           ( "Rink", Temperature.degreesCelsius -4 )
///           ( "Sauna", Temperature.degreesCelsius 82 )
///         ]
/// </code>
/// <para>then you could sort by temperature with</para>
/// <code>
///     Temperature.sortBy Tuple.second rooms
///     --&gt; [ ( "Rink", Temperature.degreesCelsius -4 )
///     --&gt;   ( "Locker room", Temperature.degreesCelsius 17 )
///     --&gt;   ( "Lobby", Temperature.degreesCelsius 21 )
///     --&gt;   ( "Sauna", Temperature.degreesCelsius 82 )
///     --&gt; ]
/// </code>
/// </example>
/// <param name="toTemperature">The mapping function.</param>
/// <param name="list">The list of items.</param>
let sortBy (toTemperature: 'a -> Temperature) (list: 'a list) : 'a list =
    let comparator first second =
        compare (toTemperature first) (toTemperature second)

    List.sortWith comparator list
