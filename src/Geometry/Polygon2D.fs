[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Polygon2D

(* Builders *)

/// Points that are each connected together. Polygon's have the assumption that the last point is connected back to
/// the first point, so it is not necessary to explicitly add it.
let singleLoop (points: Point2D<'Unit, 'Coordinates> list) : Polygon2D<'Unit, 'Coordinates> = { Points = points }

(* Accessors *)

let boundingBox (polygon: Polygon2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    BoundingBox2D.containingPoints polygon.Points (BoundingBox2D.empty ())

(* Modifiers *)

let translate (amount: Vector2D<'Unit, 'Coordinates>) (polygon: Polygon2D<'Unit, 'Coordinates>) : Polygon2D<'Unit, 'Coordinates> =
    { Points = List.map (Point2D.translate amount) polygon.Points }


let rotateAround (reference: Point2D<'Unit, 'Coordinates>) (angle: Angle) (polygon: Polygon2D<'Unit, 'Coordinates>) : Polygon2D<'Unit, 'Coordinates> =
    { Points = List.map (Point2D.rotateAround reference angle) polygon.Points }

let placeIn (frame: Frame2D<'Unit, 'Coordinates>) (polygon: Polygon2D<'Unit, 'Coordinates>) : Polygon2D<'Unit, 'Coordinates> =
    { Points = List.map (Point2D.placeIn frame) polygon.Points }
