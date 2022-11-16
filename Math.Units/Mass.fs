/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Mass</c> represents a mass in kilograms, pounds, metric or imperial tons
/// etc. It is stored as a number of kilograms.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Mass

/// Construct a mass from a number of kilograms.
let kilograms (numKilograms: float) : Mass = Quantity numKilograms

/// Convert a mass to a number of kilograms.
let inKilograms (numKilograms: Mass) : float = numKilograms.Value

/// Construct a mass from a number of grams.
let grams (numGrams: float) : Mass = kilograms (0.001 * numGrams)

/// Convert a mass to a number of grams.
let inGrams (mass: Mass) : float = 1000. * inKilograms mass

/// Construct a mass from a number of pounds.
let pounds (numPounds: float) : Mass = kilograms (Constants.pound * numPounds)

/// Convert a mass to a number of pounds.
let inPounds (mass: Mass) : float = inKilograms mass / Constants.pound

/// Construct a mass from a number of ounces.
let ounces (numOunces: float) : Mass = kilograms (Constants.ounce * numOunces)

/// <summary>
/// Convert a mass to a number of ounces.
/// <code>
///    Mass.pounds 1 |&gt; Mass.inOunces
///    --> 16
/// </code>
/// </summary>
let inOunces (mass: Mass) : float = inKilograms mass / Constants.ounce

/// <summary>
/// Construct a mass from a number of <a href="https://en.wikipedia.org/wiki/Tonne">metric tons</a>.
/// <code>
///    Mass.metricTons 1
///    --> Mass.kilograms 1000
/// </code>
/// </summary>
let metricTons (numTonnes: float) : Mass = kilograms (1000. * numTonnes)

/// Convert a mass to a number of metric tons.
let inMetricTons (mass: Mass) : float = 0.001 * inKilograms mass

/// <summary>
/// Construct a mass from a number of <a href="https://en.wikipedia.org/wiki/Short_ton">short tons</a>. This is the 'ton'
/// commonly used in the United States.
/// <code>
///     Mass.shortTons 1
///     --> Mass.pounds 2000
/// </code>
/// </summary>
let shortTons (numShortTons: float) : Mass =
    kilograms (Constants.shortTon * numShortTons)

/// Convert a mass to a number of short tons.
let inShortTons (mass: Mass) : float = inKilograms mass / Constants.shortTon

/// <summary>
/// Construct a mass from a number of
/// <a href="https://en.wikipedia.org/wiki/Long_ton">long tons</a>.
/// This is the 'ton' commonly used in the United Kingdom and British Commonwealth.
/// <code>
///     Mass.longTons 1
///     --> Mass.pounds 2240
/// </code>
/// </summary>
let longTons (numLongTons: float) : Mass =
    kilograms (Constants.longTon * numLongTons)

/// Convert a mass to a number of long tons.
let inLongTons (mass: Mass) : float = inKilograms mass / Constants.longTon

let kilogram = kilograms 1
let gram = grams 1
let metricTon = metricTons 1
let pound = pounds 1
let ounce = ounces 1
let longTon = longTons 1
let shortTon = shortTons 1
