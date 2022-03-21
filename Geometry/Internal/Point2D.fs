module internal Geometry.Internal.Point2D

open Geometry

let placeIn (frame: Frame2D<'Unit, 'Coordinates>) (point: Point2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> =
    let i = frame.XDirection
    let j = frame.YDirection

    { X = (frame.Origin.X + point.X * i.X + point.Y * j.X)
      Y = (frame.Origin.Y + point.X * i.Y + point.Y * j.Y) }
