[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.BoundingBox2D


// ---- Builders ----

/// Creates an infinitely small bounding box. This can be used when growing a bounding box around objects
let empty () : BoundingBox2D<'Unit, 'Coordinates> =
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

let singleton (p: Point2D<'Unit, 'Coordinastes>) : BoundingBox2D<'Unit, 'Coordinates> = from p p


// ---- Accessors ----

/// Returned in clockwise order from top left rotating around clockwise
let corners (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> list =
    [ bbox.TopLeft
      bbox.TopRight
      bbox.BottomRight
      bbox.BottomLeft ]

let width (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = bbox.MaxX - bbox.MinX

let height (bbox: BoundingBox2D<'Unit, 'Coordinates>) : Length<'Unit> = bbox.MaxY - bbox.MinY


// ---- Modifiers ----

/// Get a bounding box that contains the new point. If the box does not contain the new point, the box will grow
/// to fit the new point. If the point is within the box, the same bounding box is returned.
let containingPoint (point: Point2D<'Unit, 'Coordinates>) box =
    { MinX = min box.MinX point.X
      MaxX = max box.MaxX point.X
      MinY = min box.MinY point.Y
      MaxY = max box.MaxY point.Y }

let containingPoints
    (points: Point2D<'Unit, 'Coordinates> list)
    (box: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    Seq.fold (fun box point -> containingPoint point box) box points


// ---- Queries ----

/// Get the four line segments surrounding the bounding box. The lines are created from the top left point, creating
/// line segments around the bounding box clockwise.
let lineSegments (bbox: BoundingBox2D<'Unit, 'Coordinates>) =
    [ LineSegment2D.from bbox.TopLeft bbox.TopRight
      LineSegment2D.from bbox.TopRight bbox.BottomRight
      LineSegment2D.from bbox.BottomRight bbox.BottomLeft
      LineSegment2D.from bbox.BottomLeft bbox.TopLeft ]

/// Test to see if the target bounding box is contained withing the bounding box
let contains (target: BoundingBox2D<'Unit, 'Coordiantes>) (bbox: BoundingBox2D<'Unit, 'Coordiantes>) =
    target.MinX >= bbox.MinX
    && target.MaxX <= bbox.MaxX
    && target.MinY >= bbox.MinY
    && target.MaxY <= bbox.MaxY

let intersects (first: BoundingBox2D<'Unit, 'Coordiantes>) (second: BoundingBox2D<'Unit, 'Coordiantes>) =
    first.MinX <= second.MaxX
    && first.MaxX >= second.MinX
    && first.MinY <= second.MaxY
    && first.MaxY >= second.MinY

let intersection (first: BoundingBox2D<'Unit, 'Coordiantes>) (second: BoundingBox2D<'Unit, 'Coordiantes>) =
    if intersects first second then
        Some
            { MinX = max first.MinX second.MinX
              MaxX = min first.MaxX second.MaxX
              MinY = max first.MinY second.MinY
              MaxY = min first.MaxY second.MaxY }
    else
        None


/// Create a bounding box that contains both bounding boxes.
let union
    (first: BoundingBox2D<'Unit, 'Coordinates>)
    (second: BoundingBox2D<'Unit, 'Coordinates>)
    : BoundingBox2D<'Unit, 'Coordinates> =
    { MinX = min first.MinX second.MinX
      MaxX = max first.MaxX second.MaxX
      MinY = min first.MinY second.MinY
      MaxY = max first.MaxY second.MaxY }
