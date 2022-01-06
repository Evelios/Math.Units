namespace Geometry

[<Struct>]
type BoundingBox2D<'Length, 'Coordinates> =
    { MinX: float
      MaxX: float
      MaxY: float
      MinY: float }

    member this.TopLeft = Point2D.xy this.MinX this.MaxY
    member this.TopRight = Point2D.xy this.MaxX this.MaxY
    member this.BottomRight = Point2D.xy this.MaxX this.MinY
    member this.BottomLeft = Point2D.xy this.MinX this.MinY

module BoundingBox2D =
    (* Builders *)

    // Creates an infinitely small bounding box. This can be used when growing a bounding box around objects
    let empty () =
        { MinX = infinity
          MaxX = -infinity
          MinY = infinity
          MaxY = -infinity }

    /// Create a bounding box that contains the two points
    let from (p1: Point2D<'Length, 'Coordinates>) (p2: Point2D<'Length, 'Coordinates>) =
        { MinX = min p1.x p2.x
          MaxX = max p1.x p2.x
          MinY = min p1.y p2.y
          MaxY = max p1.y p2.y }
        
    let fromExtrema b: BoundingBox2D<'Length, 'Coordinates> = b

    (* Accessors *)

    /// Returned in clockwise order from top left rotating around clockwise
    let corners (bbox: BoundingBox2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> list =
        [ bbox.TopLeft
          bbox.TopRight
          bbox.BottomRight
          bbox.BottomLeft ]

    let width (bbox: BoundingBox2D<'Length, 'Coordinates>) : float = bbox.MaxX - bbox.MinX

    let height (bbox: BoundingBox2D<'Length, 'Coordinates>) : float = bbox.MaxY - bbox.MinY

    (* Modifiers *)

    /// Get a bounding box that contains the new point. If the box does not contain the new point, the box will grow
    /// to fit the new point. If the point is within the box, the same bounding box is returned.
    let containingPoint (point: Point2D<'Length, 'Coordinates>) box =
        { MinX = min box.MinX point.x
          MaxX = max box.MaxX point.x
          MinY = min box.MinY point.y
          MaxY = max box.MaxY point.y }

    let containingPoints points box =
        Seq.fold (fun box point -> containingPoint point box) box points

    (* Queries *)

    /// Get the four line segments surrounding the bounding box. The lines are created from the top left point, creating
    /// line segments around the bounding box clockwise.
    let lineSegments (bbox: BoundingBox2D<'Length, 'Coordinates>) =
        [ LineSegment2D.from bbox.TopLeft bbox.TopRight
          LineSegment2D.from bbox.TopRight bbox.BottomRight
          LineSegment2D.from bbox.BottomRight bbox.BottomLeft
          LineSegment2D.from bbox.BottomLeft bbox.TopLeft ]

    (* Actions *)
    
    /// Create a bounding box that contains both bounding boxes.
    let union (first: BoundingBox2D<'Length, 'Coordinates>) (second: BoundingBox2D<'Length, 'Coordinates>) : BoundingBox2D<'Length, 'Coordinates> =
        { MinX = min first.MinX second.MinX
          MaxX = max first.MaxX second.MaxX
          MinY = min first.MinY second.MinY
          MaxY = max first.MaxY second.MaxY }
