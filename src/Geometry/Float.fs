namespace Geometry

open System
open LanguagePrimitives

type Float() =
    static let mutable digitPrecision = 10

    static member DigitPrecision
        with get () = digitPrecision
        and set v = digitPrecision <- v
        
    static member Epsilon with get() =  10. ** (float -Float.DigitPrecision)

[<AutoOpen>]
module Float =

    let Epsilon : float = 10. ** (float -Float.DigitPrecision)
    let almostEqual (a: float) (b: float) : bool = abs (a - b) < FloatWithMeasure Epsilon
    let roundFloatTo (precision: int) (x: float) = Math.Round(x, precision)
    let roundFloat (x: float) = roundFloatTo Float.DigitPrecision x
