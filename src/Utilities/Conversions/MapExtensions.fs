namespace Utilities.Conversions

open System.Runtime.CompilerServices

[<Extension>]
type MapExtension() =

    [<Extension>]
    static member inline Keys(map: Map<'K, 'V>) = map |> Map.toSeq |> Seq.map fst
