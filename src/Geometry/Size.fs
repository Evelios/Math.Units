namespace Geometry

type Size =
    private
        { width: float
          height: float }
    member this.Width = this.width
    member this.Height = this.height

module Size =

    let empty = { width = 0.; height = 0. }
    let create width height = { width = width; height = height }

    let scale scale (size: Size) =
        { width = size.Width * scale
          height = size.Height * scale }

    let normalizeBelowOne (size: Size) =
        scale (1. / max size.Width size.Height) size

    let withMaxSize size = normalizeBelowOne >> scale size
