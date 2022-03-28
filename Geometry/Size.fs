[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Size

let empty<'Unit> () : Size2D<'Unit> =
    { Width = Length.zero
      Height = Length.zero }

let create width height : Size2D<'Unit> = { Width = width; Height = height }

let scale (scale: float) (size: Size2D<'Unit>) : Size2D<'Unit> =
    { Width = size.Width * scale
      Height = size.Height * scale }

let normalizeBelowOne (size: Size2D<'Unit>) : Size2D<'Unit> =
    scale
        (Length.create<'Unit> 1.
         / max size.Width size.Height)
        size

let withMaxSize<'Unit> (maxSize: Length<'Unit>) (size: Size2D<'Unit>) : Size2D<'Unit> =
    size
    |> normalizeBelowOne
    |> scale (Length.unpack maxSize)
