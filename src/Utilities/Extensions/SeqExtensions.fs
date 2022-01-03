namespace Utilities.Extensions

module Seq =

    /// Get the cartesian product of the two sequences
    let cartesian xs ys =
        xs
        |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))
