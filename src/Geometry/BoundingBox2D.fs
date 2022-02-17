[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.BoundingBox2D
(* Builders *)

// Creates an infinitely small bounding box. This can be used when growing a bounding box around objects
let empty<'Unit, 'Coordinates> () =
    { MinX = Length.create<'Unit> infinity
      MaxX = Length.create<'Unit> -infinity
      MinY = Length.create<'Unit> infinity
      MaxY = Length.create<'Unit> -infinity }

/// Create a bounding box that contains the two points
let from (p1: Point2D<'Unit, 'Coordinates>) (p2: Point2D<'Unit, 'Coordinates>) =
    { MinX = min p1.X p2.X
      MaxX = max p1.X p2.X
      MinY = min p1.Y p2.Y
      MaxY = max p1.Y p2.Y }

let fromExtrema b : BoundingBox2D<'Unit, 'Coordinates> = b

(* Accessors *)

/// Returned in clockwise order from top left rotating around clockwise
let corners (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> list =
    [ bbox.TopLeft
      bbox.TopRight
      bbox.BottomRight
      bbox.BottomLeft ]

let width (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = bbox.MaxX - bbox.MinX

let height (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = bbox.MaxY - bbox.MinY

(* Modifiers *)

/// Get a bounding box that contains the new point. If the box does not contain the new point, the box will grow
/// to fit the new point. If the point is within the box, the same bounding box is returned.
let containingPoint (point: Point2D<'Unit, 'Coordinates>) box =
    { MinX = min box.MinX point.X
      MaxX = max box.MaxX point.X
      MinY = min box.MinY point.Y
      MaxY = max box.MaxY point.Y }

let containingPoints points box =
    Seq.fold (fun box point -> containingPoint point box) box points

(* Queries *)

/// Get the four line segments surrounding the bounding box. The lines are created from the top left point, creating
/// line segments around the bounding box clockwise.
let lineSegments (bbox: BoundingBox2D<'Unit, 'Coordinates>) =
    [ LineSegment2D.from bbox.TopLeft bbox.TopRight
      LineSegment2D.from bbox.TopRight bbox.BottomRight
      LineSegment2D.from bbox.BottomRight bbox.BottomLeft
      LineSegment2D.from bbox.BottomLeft bbox.TopLeft ]

(* Actions *)

/// Create a bounding box that contains both bounding boxes.
let union
    (first: BoundingBox2D<'Unit, 'Coordinates>)
    (second: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = min first.MinX second.MinX
      MaxX = max first.MaxX second.MaxX
      MinY = min first.MinY second.MinY
      MaxY = max first.MaxY second.MaxY }
