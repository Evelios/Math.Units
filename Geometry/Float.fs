namespace Geometry

open System
open LanguagePrimitives

type Float() =
    static let mutable digitPrecision = 10

    static member DigitPrecision
        with get () = digitPrecision
        and set v = digitPrecision <- v

    static member Epsilon = 10. ** (float -Float.DigitPrecision)

[<AutoOpen>]
module Float =

    let almostEqual (a: float) (b: float) : bool = abs (a - b) < FloatWithMeasure Float.Epsilon
    let roundFloatTo (precision: int) (x: float) = Math.Round(x, precision)
    let roundFloat (x: float) = roundFloatTo Float.DigitPrecision x


    /// Interpolate from the first value to the second, based on a parameter that
    /// ranges from zero to one. Passing a parameter value of zero will return the start
    /// value and passing a parameter value of one will return the end value.
    ///     Float.interpolateFrom 5 10 0
    ///     --> 5
    ///     Float.interpolateFrom 5 10 1
    ///     --> 10
    ///     Float.interpolateFrom 5 10 0.6
    ///     --> 8
    /// The end value can be less than the start value:
    ///     Float.interpolateFrom 10 5 0.1
    ///     --> 9.5
    /// Parameter values less than zero or greater than one can be used to extrapolate:
    ///     Float.interpolateFrom 5 10 1.5
    ///     --> 12.5
    ///     Float.interpolateFrom 5 10 -0.5
    ///     --> 2.5
    ///     Float.interpolateFrom 10 5 -0.2
    ///     --> 11
    /// -}
    let interpolateFrom (start: float) (finish: float) (parameter: float) : float =
        if parameter <= 0.5 then
            start + parameter * (finish - start)

        else
            finish + (1. - parameter) * (start - finish)
