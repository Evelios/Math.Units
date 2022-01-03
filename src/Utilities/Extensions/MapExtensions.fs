namespace Utilities.Extensions

module Map =
    let keys map = Map.toSeq map |> Seq.map fst

    let values map = Map.toSeq map |> Seq.map snd
