/// Although most of the focus of `elm-units` is on physical/scientific units,
/// it's often useful to be able to safely convert back and forth between (for
/// example) [`Length`](Length) values in the real world and on-screen lengths in
/// pixels.
/// This module provides a standard `Pixels` units type and basic functions for
/// constructing/converting values of type `Quantity Int Pixels` or
/// `Quantity Float Pixels`, which allows you to do things like represent
/// conversions between real-world and on-screen lengths as [rates of change][1].
/// This in turn means that all the normal [`Quantity`](Quantity) functions can be
/// used to convert between pixels and other units, or even do type-safe math
/// directly on pixel values.
/// [1]: Quantity#working-with-rates
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Pixels

/// Construct a quantity representing a floating-point number of on-screen
/// pixels:
///     lineWeight =
///         Pixels.float 1.5
let float (numPixels: float) : Pixels = Quantity numPixels

/// Convert a floating-point number of pixels back into a plain `Float`:
///    pixelDensity =
///        Pixels.float 96 |> Quantity.per (Length.inches 1)
///    Length.centimeters 1
///        |> Quantity.at pixelDensity
///        |> Pixels.toFloat
///    --> 37.795275590551185
let toFloat (numPixels: Pixels) : float = numPixels.Value

/// Shorthand for `Pixels.float 1.`. Can be convenient to use with
/// [`Quantity.per`](Quantity#per).
let pixel = float 1.

/// Construct an on-screen speed from a number of pixels per second.
let pixelsPerSecond (numPixelsPerSecond: float) : Quantity<PixelsPerSecond> = Quantity numPixelsPerSecond

/// Convert an on-screen speed to a number of pixels per second.
///     elapsedTime =
///         Duration.milliseconds 16
///     dragDistance =
///         Pixels.float 2
///     dragSpeed =
///         dragDistance |> Quantity.per elapsedTime
///     dragSpeed |> Pixels.inPixelsPerSecond
///     --> 125
let inPixelsPerSecond (numPixelsPerSecond: Quantity<PixelsPerSecond>) : float = numPixelsPerSecond.Value

/// Construct an on-screen acceleration from a number of pixels per second
/// squared.
let pixelsPerSecondSquared (numPixelsPerSecondSquared: float) : Quantity<PixelsPerSecondSquared> =
    Quantity numPixelsPerSecondSquared

/// Convert an on-screen acceleration to a number of pixels per second squared.
let inPixelsPerSecondSquared (numPixelsPerSecondSquared: Quantity<PixelsPerSecondSquared>) : float =
    numPixelsPerSecondSquared.Value


/// Construct an on-screen area from a number of square pixels.
let squarePixels (numSquarePixels: float) : Quantity<SquarePixels> = Quantity numSquarePixels

/// Convert an on-screen area to a number of square pixels.
///    area =
///        Pixels.int 1928 |> Quantity.times (Pixels.int 1080)
///    area |> Pixels.inSquarePixels
///    --> 2073600
let inSquarePixels (numSquarePixels: Quantity<SquarePixels>) : float = numSquarePixels.Value
