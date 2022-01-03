module UtilitiesTests.Extensions.ListExtensionsTests

open NUnit.Framework
open Utilities.Extensions

[<SetUp>]
let Setup () = ()

[<Test>]
let Pairs() =
    let given = [ 1; 2; 3 ]
    let expected = [ 1, 2; 1, 3; 2, 3 ]
    let actual = List.pairs given
    Assert.AreEqual(expected, actual)
