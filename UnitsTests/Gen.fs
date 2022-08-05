namespace UnitsTests

type 'a Positive = Positive of 'a
type 'a ZeroToOne = ZeroToOne of 'a

module Gen =

    open System
    open FsCheck

    open Units

    /// Generates a random floating point number from [0.0, 1.0), not including
    /// 1.0.
    let rand: Gen<float> =
        Gen.choose (0, Int32.MaxValue)
        |> Gen.map (fun x -> float x / (float Int32.MaxValue))

    /// Generate a random integer value in the range [low, high] inclusive of
    /// both the lower and upper limit.
    let intBetween (low: int) (high: int) = Gen.choose (low, high)

    /// Generate a random float value in the range [low, high] inclusive of
    /// both the lower and upper limit.
    let floatBetween (low: float) (high: float) : Gen<float> =
        Gen.map (fun scale -> (low + (high - low)) * scale) rand

    let zeroToOneFloat: Gen<float ZeroToOne> =
        Gen.map ZeroToOne (floatBetween 0. 1.)

    /// Generates a normal floating point number. This function excludes certain
    /// values from being generated as a float. The following are not included
    /// when generating a float: `-infinity`, `infinity`, and `NaN`.
    let float: Gen<float> =
        Arb.generate<NormalFloat> |> Gen.map float

    /// Generate a floating point number int the range [0, infinity). This
    /// generates `0.` values and other positive floating point numbers.
    let positiveFloat: Gen<float> =
        Gen.map abs float

    /// Generate a random `Quantity` value.
    let quantity<'Units> : Gen<Quantity<'Units>> =
        Gen.map Quantity float

    /// Generate a `Positive<Length>` values. This is a type safe way of
    /// generating and enforcing positive `Length` values.
    let positiveQuantity<'Units> : Gen<Positive<Quantity<'Units>>> =
        Gen.map (Quantity >> Positive) positiveFloat

    /// Generate a random quantity value within a given range.
    let quantityBetween (low: Quantity<'Units>) (high: Quantity<'Units>) : Gen<Quantity<'Units>> =

        Gen.map Quantity (floatBetween low.Value high.Value)

    let interval<'Units> : Gen<Interval<'Units>> =
        Gen.map2 Interval.from quantity<'Units> quantity<'Units>

    type ArbGeometry =
        static member Float() = Arb.fromGen float
        static member Register() = Arb.register<ArbGeometry> () |> ignore
        static member Quantity() = Arb.fromGen quantity
        static member Interval() = Arb.fromGen interval
        static member ZeroToOneFloat() = Arb.fromGen zeroToOneFloat

module Arb =
    open FsCheck

    open Units

    let floatBetween start finish : Arbitrary<float> =
        Gen.floatBetween start finish |> Arb.fromGen

    let quantity<'Units> : Arbitrary<Quantity<'Units>> =
        Gen.quantity |> Arb.fromGen

    let interval<'Units> : Arbitrary<Interval<'Units>> =
        Gen.interval |> Arb.fromGen
