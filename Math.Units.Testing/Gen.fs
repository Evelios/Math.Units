namespace Math.Units.Test

/// <summary>Represents a positive value of type 'a.</summary>
/// <param name="Item">The underlying value.</param>
type 'a Positive = Positive of Item: 'a
/// <summary>Represents a value of type 'a in the range [0, 1].</summary>
/// <param name="Item">The underlying value.</param>
type 'a ZeroToOne = ZeroToOne of Item: 'a

/// <summary>
/// Fuzz testing generator for components created in the <c>Math.Units</c> module.
/// </summary>
module Gen =

    open System
    open FsCheck

    open Math.Units

    /// <summary>
    /// Generates a random floating point number from [0.0, 1.0], not including
    /// 1.0.
    /// </summary>
    let rand: Gen<float> =
        Gen.choose (0, Int32.MaxValue)
        |> Gen.map (fun x -> float x / (float Int32.MaxValue))

    /// <summary>
    /// Generate a random integer value in the range [low, high] inclusive of
    /// both the lower and upper limit.
    /// </summary>
    /// <param name="low">The lower bound.</param>
    /// <param name="high">The upper bound.</param>
    let intBetween (low: int) (high: int) = Gen.choose (low, high)

    /// <summary>
    /// Generate a random float value in the range [low, high] inclusive of
    /// both the lower and upper limit.
    /// </summary>
    /// <param name="low">The lower bound.</param>
    /// <param name="high">The upper bound.</param>
    let floatBetween (low: float) (high: float) : Gen<float> =
        Gen.map (fun scale -> (low + (high - low)) * scale) rand

    /// <summary>
    /// Generate a float between [0. and 1.]
    /// </summary>
    let zeroToOneFloat: Gen<float ZeroToOne> =
        Gen.map ZeroToOne (floatBetween 0. 1.)

    /// <summary>
    /// Generates a normal floating point number. This function excludes certain
    /// values from being generated as a float. The following are not included
    /// when generating a float: <c>-infinity</c>, <c>infinity</c>, and <c>NaN</c>.
    /// </summary>
    let float: Gen<float> =
        Arb.generate<NormalFloat> |> Gen.map float

    /// <summary>
    /// Generate a floating point number int the range [0, infinity]. This
    /// generates <c>0.</c> values and other positive floating point numbers.
    /// </summary>
    let positiveFloat: Gen<float> =
        Gen.map abs float

    /// <summary>
    /// Generate a random <c>Quantity</c> value.
    /// </summary>
    let quantity<'Units> : Gen<Quantity<'Units>> =
        Gen.map Quantity float

    /// <summary>Generate a random temperature.</summary>
    let temperature: Gen<Temperature> =
        Gen.map Temperature.kelvins positiveFloat

    /// <summary>
    /// Generate a <c>Positive&lt;Length&gt;</c> values. This is a type safe way of
    /// generating and enforcing positive <c>Length</c> values.
    /// </summary>
    let positiveQuantity<'Units> : Gen<Positive<Quantity<'Units>>> =
        Gen.map (Quantity >> Positive) positiveFloat

    /// <summary>
    /// Generate a random quantity value within a given range.
    /// </summary>
    /// <param name="low">The lower bound.</param>
    /// <param name="high">The upper bound.</param>
    let quantityBetween (low: Quantity<'Units>) (high: Quantity<'Units>) : Gen<Quantity<'Units>> =

        Gen.map Quantity (floatBetween low.Value high.Value)

    /// <summary>Generate a random interval.</summary>
    let interval<'Units> : Gen<Interval<'Units>> =
        Gen.map2 Interval.from quantity<'Units> quantity<'Units>

    /// <summary>FsCheck configuration for Math.Units types.</summary>
    type ArbGeometry =
        /// <summary>Arbitrary float.</summary>
        static member Float() = Arb.fromGen float
        /// <summary>Register Arbitrary instances.</summary>
        static member Register() = Arb.register<ArbGeometry> () |> ignore
        /// <summary>Arbitrary quantity.</summary>
        static member Quantity() = Arb.fromGen quantity
        /// <summary>Arbitrary temperature.</summary>
        static member Temperature() = Arb.fromGen temperature
        /// <summary>Arbitrary interval.</summary>
        static member Interval() = Arb.fromGen interval
        /// <summary>Arbitrary zero-to-one float.</summary>
        static member ZeroToOneFloat() = Arb.fromGen zeroToOneFloat

/// <summary>FsCheck Arbitrary instances for Math.Units types.</summary>
module Arb =
    open FsCheck

    open Math.Units

    /// <summary>Arbitrary float.</summary>
    let float: Arbitrary<float> =
        Arb.fromGen Gen.float

    /// <summary>Arbitrary float between two values.</summary>
    /// <param name="start">The start value.</param>
    /// <param name="finish">The end value.</param>
    let floatBetween start finish : Arbitrary<float> =
        Gen.floatBetween start finish |> Arb.fromGen

    /// <summary>Arbitrary quantity.</summary>
    let quantity<'Units> : Arbitrary<Quantity<'Units>> =
        Gen.quantity |> Arb.fromGen

    /// <summary>Arbitrary temperature.</summary>
    let temperature: Arbitrary<Temperature> =
        Gen.temperature |> Arb.fromGen

    /// <summary>Arbitrary interval.</summary>
    let interval<'Units> : Arbitrary<Interval<'Units>> =
        Gen.interval |> Arb.fromGen
