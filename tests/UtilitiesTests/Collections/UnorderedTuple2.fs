module UtilitiesTests.Collections.UnorderedTuple2

open NUnit.Framework
open FsCheck.NUnit
open Utilities.Collections
open Utilities.Test

[<SetUp>]
let SetUp () = ()


[<Property>]
let ``Order agnostic equality integers`` (a: int) (b: int) =
    UnorderedTuple2.from a b
    .=. UnorderedTuple2.from b a
    
[<Property>]
let ``Order agnostic equality strings`` (a: string) (b: string) =
    UnorderedTuple2.from a b
    .=. UnorderedTuple2.from b a

[<Property>]
let ``Order agnostic hash equality`` (a: int) (b: int) =
    (UnorderedTuple2.from a b).GetHashCode()
    .=. (UnorderedTuple2.from b a).GetHashCode()
