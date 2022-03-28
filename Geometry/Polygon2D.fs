[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Polygon2D

// ---- Builders ----

/// Points that are each connected together. Polygon's have the assumption that the last point is connected back to
/// the first point, so it is not necessary to explicitly add it.
let singleLoop (points: Point2D<'Unit, 'Coordinates> list) : Polygon2D<'Unit, 'Coordinates> = { OuterLoop = points ; InnerLoops = []}

// ---- Accessors ----

let boundingBox (polygon: Polygon2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    BoundingBox2D.containingPoints polygon.OuterLoop (BoundingBox2D.empty ())

// ---- Modifiers ----

let map
    (f: Point2D<'UnitA, 'CoordinatesA> -> Point2D<'UnitB, 'CoordinatesB>)
    (polygon: Polygon2D<'UnitA, 'CoordinatesA>)
    : Polygon2D<'UnitB, 'CoordinatesB> =
    { OuterLoop = List.map f polygon.OuterLoop
      InnerLoops = List.map (List.map f) polygon.InnerLoops }


let translate
    (amount: Vector2D<'Unit, 'Coordinates>)
    (polygon: Polygon2D<'Unit, 'Coordinates>)
    : Polygon2D<'Unit, 'Coordinates> =

    map (Point2D.translate amount) polygon

let rotateAround
    (reference: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (polygon: Polygon2D<'Unit, 'Coordinates>)
    : Polygon2D<'Unit, 'Coordinates> =

    map (Point2D.rotateAround reference angle) polygon

let placeIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (polygon: Polygon2D<'Unit, 'Coordinates>)
    : Polygon2D<'Unit, 'Coordinates> =

    map (Point2D.placeIn frame) polygon
