namespace Geometry

type Size<'Length> =
    private
        { width: float
          height: float }
    member this.Width = this.width
    member this.Height = this.height

module Size =

    let empty = { width = 0.; height = 0. }
    let create width height = { width = width; height = height }

    let scale scale (size: Size<'Length>) =
        { width = size.Width * scale
          height = size.Height * scale }

    let normalizeBelowOne (size: Size<'Length>) =
        scale (1. / max size.Width size.Height) size

    let withMaxSize<'Length> size = normalizeBelowOne >> scale size
