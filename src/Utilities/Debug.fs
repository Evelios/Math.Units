module Utilities.Debug

let log name x =
    printfn $"%s{name}: %A{x}"
    x

let print name x = printfn $"%s{name}: %A{x}"
