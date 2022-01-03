module UtilitiesTests.StringMapTests

open NUnit.Framework
open Utilities.Collections

[<SetUp>]
let Setup () = ()

type Enum =
    | One = 1
    | Two = 2
    | Three = 3

let stringMap =
    StringMap [ Enum.One, "One"
                Enum.Two, "Two"
                Enum.Three, "Three" ]


[<Test>]
let toString() =
    Assert.That(stringMap.ToString(Enum.One), Is.EqualTo("One"))

[<Test>]
let fromString() =
    Assert.That(stringMap.FromString("One"), Is.EqualTo(Enum.One))
