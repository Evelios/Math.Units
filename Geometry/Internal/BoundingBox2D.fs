module internal Geometry.Internal.BoundingBox2D

open Geometry

open Units

let from (firstPoint: Point2D<'Unit, 'Coordinates>) (secondPoint: Point2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    let x1 = firstPoint.X
    let y1 = firstPoint.Y
    let x2 = secondPoint.X
    let y2 = secondPoint.Y
    { MinX = Quantity.min x1 x2
      MaxX = Quantity.max x1 x2
      MinY = Quantity.min y1 y2
      MaxY = Quantity.max y1 y2
    }