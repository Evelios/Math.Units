namespace Utilities

open System
open FsCheck
open Utilities.Extensions

module Gen =
    let map7 fn a b c d e f g =
        Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.map fn a) b) c) d) e) f) g

    /// Generates a random number from [0.0, 1.0]
    let rand =
        Gen.choose (0, Int32.MaxValue)
        |> Gen.map (fun x -> float x / (float Int32.MaxValue))

    let floatBetween low high =
        Gen.map (fun scale -> (low + (high - low)) * scale) rand

    let float =
        Arb.generate<NormalFloat> |> Gen.map float

    let string =
        Arb.generate<XmlEncodedString>
        |> Gen.map string
        |> Gen.filter (fun str -> str <> null)


    let ofType<'a> =
        Gen.elements DiscriminatedUnion.allCases<'a>

    let setOfType<'a when 'a: comparison> =
        Seq.toList DiscriminatedUnion.allCases<'a>
        |> Gen.elements
        |> Gen.listOf
        |> Gen.map Set.ofList
