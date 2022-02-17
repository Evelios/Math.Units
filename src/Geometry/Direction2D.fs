[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Direction2D

(* Constants *)

let xy x y =
    { Direction2D.X = x; Direction2D.Y = y }

let positiveX () : Direction2D<'Coordinates> = xy 1.0 0.0
let positiveY () : Direction2D<'Coordinates> = xy 0. 1.
let negativeX () : Direction2D<'Coordinates> = xy -1. 0.
let negativeY () : Direction2D<'Coordinates> = xy 0. -1.
let x = positiveX
let y = positiveY

(* Builders *)

// Create an angle counterclockwise from the positive X direction.
let fromAngle (angle: Angle) = xy (Angle.cos angle) (Angle.sin angle)

(* Modifiers *)

// Rotate a direction by 90 degrees counterclockwise.
let rotateCounterclockwise (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    xy -direction.Y direction.X

let placeIn
    (reference: Frame2D<'Unit, 'Coordinates>)
    (direction: Direction2D<'Coordinates>)
    : Direction2D<'Coordiantes> =
    let dx = reference.XDirection
    let dy = reference.YDirection

    { X = direction.X * dx.X + direction.Y * dy.X
      Y = direction.X * dx.Y + direction.Y * dy.Y }
