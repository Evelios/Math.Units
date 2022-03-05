[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Size

let empty<'Unit> () : Size<'Unit> =
    { Width = Length.zero
      Height = Length.zero }

let create width height : Size<'Unit> = { Width = width; Height = height }

let scale (scale: float) (size: Size<'Unit>) : Size<'Unit> =
    { Width = size.Width * scale
      Height = size.Height * scale }

let normalizeBelowOne (size: Size<'Unit>) : Size<'Unit> =
    scale
        (Length.create<'Unit> 1.
         / max size.Width size.Height)
        size

let withMaxSize<'Unit> (maxSize: Length<'Unit>) (size: Size<'Unit>) : Size<'Unit> =
    size
    |> normalizeBelowOne
    |> scale (Length.unpack maxSize)
