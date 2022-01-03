namespace Utilities.Collections

open System

[<Measure>]
type fraction

[<Measure>]
type ratio

type Percentage =
    private
    | Ratio of float<ratio>
    override this.ToString() =
        match this with
        | Ratio ratio -> Math.Round(ratio * 100.<_>, 2).ToString() + "%"


module Percentage =
    let private fractionOfRatio = 100.<fraction/ratio>

    (* Builders *)
    let ofFraction fraction = Ratio(fraction / fractionOfRatio)

    let ofRatio ratio = Ratio ratio
    let ofFloat (ratio: float) = Ratio(ratio * 1.<_>)

    (* Accessors *)
    let fraction (Ratio ratio) = ratio * fractionOfRatio

    let ratio (Ratio ratio) = ratio
    let float (Ratio ratio) = float ratio
