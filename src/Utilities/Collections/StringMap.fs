namespace Utilities.Collections

type StringMap<'a when 'a: comparison>(items: ('a * string) list) =
    let itemIsKey = Map.ofList items

    let stringIsKey =
        Map.ofList (List.map (fun (a, b) -> (b, a)) items)

    member this.ToString key = Map.find key itemIsKey
    member this.TryToString key = Map.tryFind key itemIsKey
    member this.FromString key = Map.find key stringIsKey
    member this.TryFromString key = Map.tryFind key stringIsKey
