namespace Utilities.Conversions

open FSharp.Json

module Transforms =
    [<AbstractClass>]
    type ISetTransform<'T when 'T: comparison>() =
        abstract member convertToString: 'T -> string
        abstract member convertFromString: string -> 'T
        
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<string list>) ()
            member this.toTargetType value =
                (value :?> 'T Set)
                |> Set.toList
                |> List.map this.convertToString
                :> obj

            member this.fromTargetType value =
                value :?> string list
                |> List.map this.convertFromString
                |> Set.ofList
                :> obj