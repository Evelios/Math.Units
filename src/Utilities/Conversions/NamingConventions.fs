namespace Utilities.Conversions

module NamingConventions =
    open System.Text.RegularExpressions

    /// Converts camel case input into space separated camel case input
    let upperCaseSpaceSeparated (input: string) : string =
        Regex.Replace(input, "(\\B[A-Z])", " $1")
