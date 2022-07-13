namespace UnitsTests

module Gen =
    
    open System
    open FsCheck

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


    type ArbGeometry =
        static member Float() = Arb.fromGen float
        static member Register() = Arb.register<ArbGeometry> () |> ignore
