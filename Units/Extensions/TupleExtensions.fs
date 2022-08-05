namespace FSharp.Extensions

module Tuple2 =
    let pair x y = x, y

    let replicate x = x, x

    let curry f x y = f (x, y)

    let uncurry f (x, y) = f x y

    let swap (x, y) = (y, x)

    let map f (x, y) = f x y

    let mapFst f (x, y) = f x, y

    let mapSnd f (x, y) = x, f y
    let mapBoth f (x, y) = f x, f y

    let extendFst f (x, y) = f (x, y), y

    let extendSnd f (x, y) = x, f (x, y)

    let optionOfFst f (x, y) =
        match f x with
        | Some x' -> Some(x', y)
        | None -> None

    let optionOfSnd f (x, y) =
        match f y with
        | Some y' -> Some(x, y')
        | None -> None

    let toList (x, y) = [ x; y ]

module Tuple3 =
    let map f (x, y, z) = f x y z
