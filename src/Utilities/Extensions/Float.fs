namespace Utilities.Extensions

module Float =
    let remap (fromStart, fromEnd) (toStart, toEnd) value : float =
        toStart
        + (value - fromStart) * (toEnd - toStart)
          / (fromEnd - fromStart)

    let remapToInt (fromStart: float, fromEnd: float) (toStart: int, toEnd: int) value : int =
        (float toStart)
        + (value - fromStart)
          * (float toEnd - float toStart)
          / (fromEnd - fromStart)
        |> int
        |> max toStart
        |> min toEnd
