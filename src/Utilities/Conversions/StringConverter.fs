namespace Utilities.Conversions

module StringConverter =

    let private TryParseWith (tryParseFunc: string -> bool * _) =
        tryParseFunc
        >> function
        | true, v -> Some v
        | false, _ -> None

    let ParseInt = TryParseWith System.Int32.TryParse
