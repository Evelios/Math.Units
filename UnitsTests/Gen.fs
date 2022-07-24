namespace UnitsTests

type 'a Positive = Positive of 'a

module Gen =

    open System
    open FsCheck

    open Units

    /// Generates a random number from [0.0, 1.0]
    let rand =
        Gen.choose (0, Int32.MaxValue)
        |> Gen.map (fun x -> float x / (float Int32.MaxValue))

    let intBetween low high = Gen.choose (low, high)

    let floatBetween low high =
        Gen.map (fun scale -> (low + (high - low)) * scale) rand

    let float =
        Arb.generate<NormalFloat> |> Gen.map float

    let positiveFloat = Gen.map abs float

    let angle = Gen.map Angle.radians float

    let length = Gen.map Length.meters float

    let positiveLength: Gen<Length Positive> =
        Gen.map (Length.meters >> Positive) positiveFloat

    let lengthBetween (a: Length) (b: Length) : Gen<Length> =
        Gen.map Length.meters (floatBetween a.Value b.Value)

    type ArbGeometry =
        static member Float() = Arb.fromGen float
        static member Register() = Arb.register<ArbGeometry> () |> ignore
        static member Angle() = Arb.fromGen angle
        static member Length() = Arb.fromGen length
