module internal Geometry.Internal.Vector2D

open Geometry

let componentIn (d: Direction2D<'Coordinates>) (v: Vector2D<'Unit, 'Coordiantes>) : Length<'Unit> =
    (v.X * d.X + v.Y * d.Y)
