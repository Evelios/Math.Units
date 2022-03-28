[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Polyline2D

let fromVertices (vertices: Point2D<'Unit, 'Coordinates> list): Polyline2D<'Unit, 'Coordinates> =
    Polyline2D.Polyline2D vertices

