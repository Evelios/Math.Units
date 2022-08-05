module internal Geometry.Internal.Point2D

open Geometry

open Units

let placeIn<'Units, 'GlobalCoordinates, 'Defines, 'LocalCoordinates>
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (point: Point2D<'Units, 'GlobalCoordinates>)
    : Point2D<'Units, 'LocalCoordinates> =
    let i = frame.XDirection
    let j = frame.YDirection

    { X = (frame.Origin.X + point.X * i.X + point.Y * j.X)
      Y = (frame.Origin.Y + point.X * i.Y + point.Y * j.Y) }

let rotateAround
    (reference: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (point: Point2D<'Units, 'Coordinates>)
    : Point2D<'Units, 'Coordinates> =

    let c = Angle.cos angle
    let s = Angle.sin angle
    let deltaX = point.X - reference.X
    let deltaY = point.Y - reference.Y

    { X = reference.X + c * deltaX - s * deltaY
      Y = reference.Y + s * deltaX + c * deltaY }

let translateBy (v: Vector2D<'Units, 'Coordiantes>) (p: Point2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
    { X = p.X + v.X; Y = p.Y + v.Y }

let mirrorAcross (axis: Axis2D<'Units, 'Coordinates>) (p: Point2D<'Units, 'Corodinates>) : Point2D<'Units, 'Coordinates> =
    let d = axis.Direction
    let p0 = axis.Origin
    let a = 1. - 2. * d.Y * d.Y
    let b = 2. * d.X * d.Y
    let c = 1. - 2. * d.X * d.X
    let deltaX = p.X - p0.X
    let deltaY = p.Y - p0.Y

    { X = p0.X + a * deltaX + b * deltaY
      Y = p0.Y + b * deltaX + c * deltaY }

let relativeTo<'Units, 'GlobalCoordinates, 'Defines, 'LocalCoordinates>
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (p: Point2D<'Units, 'GlobalCoordinates>)
    : Point2D<'Units, 'LocalCoordinates> =
    let p0 = frame.Origin
    let i = frame.XDirection
    let j = frame.YDirection
    let deltaX = p.X - p0.X
    let deltaY = p.Y - p0.Y

    { X = deltaX * i.X + deltaY * i.Y
      Y = deltaX * j.X + deltaY * j.Y }
