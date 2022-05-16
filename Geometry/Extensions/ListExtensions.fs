module internal FSharp.Extensions.List

/// Append the first list if the condition is met
let concatIf condition first second : 'a list =
    if condition then
        second @ first
    else
        second

/// Append the first list if the condition is met
let appendIf condition first second : 'a list =
    if condition then
        first @ second
    else
        second

let appendWhenSome maybeElement list : 'a list =
    match maybeElement with
    | Some element -> list @ [ element ]
    | None -> list

let consWhenSome maybeElement list : 'a list =
    match maybeElement with
    | Some e -> e :: list
    | None -> list

/// Perform a mapping operation on a list and filter out all values that are None
let filterMap f list : 'a list =
    List.fold
        (fun acc a ->
            match f a with
            | Some b -> b :: acc
            | None -> acc)
        []
        list
    |> List.rev

/// Get the cartesian product of the two lists
let cartesian xs ys =
    xs
    |> List.collect (fun x -> ys |> List.map (fun y -> x, y))

/// Get all unique pairs of a list
let rec pairs l =
    match l with
    | h :: t ->
        [ for x in t do
            yield h, x
          yield! pairs t ]
    | _ -> []

let filterNone xs = List.choose id xs
