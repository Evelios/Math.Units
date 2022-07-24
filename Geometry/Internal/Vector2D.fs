module internal Geometry.Internal.Vector2D

open Geometry

open Units

let componentIn (d: Direction2D<'Coordinates>) (v: Vector2D<'Unit, 'Coordiantes>) : Quantity<'Unit> =
    (v.X * d.X + v.Y * d.Y)

let withQuantity (a: Quantity<'Unit>) (d: Direction2D<'Coordinates>) : Vector2D<'Unit, 'Coordiantes> =
    { X = (a * d.X); Y = (a * d.Y) }
