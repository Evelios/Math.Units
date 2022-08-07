[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Size2D

open Units

// ---- Builders ----

let empty<'Units> : Size2D<'Units> =
    { Width = Quantity.zero
      Height = Quantity.zero }

let create width height : Size2D<'Units> = { Width = width; Height = height }

// ---- Modifiers ----

let scale (scale: float) (size: Size2D<'Units>) : Size2D<'Units> =
    { Width = size.Width * scale
      Height = size.Height * scale }

let normalizeBelowOne (size: Size2D<'Units>) : Size2D<'Units> =
    scale (Quantity.create 1. / max size.Width size.Height) size

let withMaxSize<'Units> (maxSize: Length) (size: Size2D<'Units>) : Size2D<'Units> =
    size |> normalizeBelowOne |> scale maxSize.Value
