module internal Geometry.Internal.BoundingBox2D

open Geometry

let from (firstPoint: Point2D<'Unit, 'Coordinates>) (secondPoint: Point2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    let x1 = firstPoint.X
    let y1 = firstPoint.Y
    let x2 = secondPoint.X
    let y2 = secondPoint.Y
    { MinX = Length.min x1 x2
      MaxX = Length.max x1 x2
      MinY = Length.min y1 y2
      MaxY = Length.max y1 y2
    }