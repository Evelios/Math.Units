[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Duration

// let from (startTime) (endTime): Duration =
//     let
//         numMilliseconds =
//             Time.posixToMillis endTime - Time.posixToMillis startTime
//     in
//     milliseconds (toFloat numMilliseconds)


/// Construct a <c>1</c from a given number of seconds.
let seconds (numSeconds: float) : Duration = Quantity numSeconds


/// Convert a <c>1</c> to a value in seconds.
///    Duration.milliseconds 10 |> Duration.inSeconds
///    --> 0.01
let inSeconds (numSeconds: Duration) : float = numSeconds.Value

/// Construct a <c>Duration</c> from a given number of milliseconds.
///    Duration.milliseconds 5000
///    --> Duration.seconds 5
let milliseconds (numMilliseconds: float) : Duration = seconds (0.001 * numMilliseconds)


/// Convert a `Duration` to a value in milliseconds.
///    Duration.seconds 0.5 |> Duration.inMilliseconds
///    --> 500
let inMilliseconds (duration: Duration) : float = inSeconds duration * 1000.


/// Construct a `Duration` from a given number of minutes.
///    Duration.minutes 3
///    --> Duration.seconds 180
let minutes (numMinutes: float) : Duration = seconds (60. * numMinutes)


/// Convert a `Duration` to a value in minutes.
///    Duration.seconds 90 |> Duration.inMinutes
///    --> 1.5
let inMinutes (duration: Duration) : float = inSeconds duration / 60.

/// Construct a `Duration` from a given number of hours.
///    Duration.hours 1
///    --> Duration.seconds 3600
let hours (numHours: float) : Duration = seconds (Constants.hour * numHours)


/// Convert a `Duration` to a value in hours.
///    Duration.minutes 120 |> Duration.inHours
///    --> 2
let inHours (duration: Duration) : float = inSeconds duration / Constants.hour

/// Construct a `Duration` from a given number of days. A day is defined as
/// exactly 24 hours or 86400 seconds. Therefore, it is only equal to the length of
/// a given calendar day if that calendar day does not include either a leap second
/// or any added/removed daylight savings hours.
///     Duration.days 1
///     --> Duration.hours 24
let days (numDays: float) : Duration = seconds (Constants.day * numDays)

/// Convert a `Duration` to a value in days.
///    Duration.hours 72 |> Duration.inDays
///    --> 3
let inDays (duration: Duration) : float = inSeconds duration / Constants.day

/// Construct a `Duration` from a given number of weeks.
///    Duration.weeks 1
///    --> Duration.days 7
let weeks (numWeeks: float) : Duration = seconds (Constants.week * numWeeks)

/// Convert a `Duration` to a value in weeks.
///    Duration.days 28 |> Duration.inWeeks
///    --> 4
let inWeeks (duration: Duration) : float = inSeconds duration / Constants.week

/// Construct a `Duration` from a given number of [Julian years][julian_year].
/// A Julian year is defined as exactly 365.25 days, the average length of a year in
/// the historical Julian calendar. This is 10 minutes and 48 seconds longer than
/// a Gregorian year (365.2425 days), which is the average length of a year in the
/// modern Gregorian calendar, but the Julian year is a bit easier to remember and
/// reason about and has the virtue of being the 'year' value used in the definition
/// of a [light year](Length#lightYears).
///     Duration.julianYears 1
///     --> Duration.days 365.25
/// [julian_year]: https://en.wikipedia.org/wiki/Julian_year_(astronomy) "Julian year"
let julianYears (numJulianYears: float) : Duration =
    seconds (Constants.julianYear * numJulianYears)

/// Convert a `Duration` to a value in Julian years.
///    Duration.hours 10000 |> Duration.inJulianYears
///    --> 1.1407711613050422
let inJulianYears (duration: Duration) : float =
    inSeconds duration / Constants.julianYear

// addTo : Time.Posix -> Duration -> Time.Posix
// addTo time duration =
//     Time.millisToPosix
//         (Time.posixToMillis time + round (inMilliseconds duration))


// subtractFrom : Time.Posix -> Duration -> Time.Posix
// subtractFrom time duration =
//     Time.millisToPosix
//         (Time.posixToMillis time - round (inMilliseconds duration))


/// second : Duration
let second = seconds 1

/// millisecond : Duration
let millisecond = milliseconds 1

/// minute : Duration
let minute = minutes 1

/// hour : Duration
let hour = hours 1

/// day : Duration
let day = days 1

/// week : Duration
let week = weeks 1

/// julianYear : Duration
let julianYear = julianYears 1
