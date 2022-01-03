namespace Utilities.Extensions

module String =

    /// Try parsing a string into a floating point number. Will return None on failure
    let parseFloat (s: string) =
        try
            Some(float s)
        with
        | _ -> None
