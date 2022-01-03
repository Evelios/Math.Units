namespace Utilities.Extensions

open Microsoft.FSharp.Reflection

module DiscriminatedUnion =

    /// Get all the cases of a discriminated union.
    let allCases<'T> =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Seq.map (fun x -> FSharpValue.MakeUnion(x, Array.zeroCreate (x.GetFields().Length)) :?> 'T)

    /// Convert a discriminated union case to a string.
    let toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    /// Convert a string to a singleton discriminated union case.
    let fromString<'a> (s: string) =
        match FSharpType.GetUnionCases typeof<'a>
              |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

    /// Create a map from a discriminated union type where the key is the index of the case.
    let asIndexedMap<'T when 'T: comparison> () : Map<'T, int> =
        allCases<'T>
        |> Seq.mapi (fun i e -> e, i)
        |> Seq.toList
        |> Map.ofList

    /// Get the discriminated union case at a particular index.
    let fromIndex<'T> (index: int) : 'T = Seq.item index allCases<'T>
