[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Direction2D


(* Builders *)

/// Get a direction vector from the x and y components. This function takes
/// care of normalizing the x and y components into the unit direction vector.
/// This function also checks for the edge case where the x and y components
/// are both zero. In that case, the function returns `None`.
let xy (x: float) (y: float) : Direction2D<'Coordinates> option =
    if x = 0. && y = 0. then
        None
    else
        let magnitude = sqrt ((x * x) + (y * y))

        Some
            { Direction2D.X = x / magnitude
              Direction2D.Y = y / magnitude }

/// Get a direction vector from the x and y length components. This function takes
/// care of normalizing the x and y components into the unit direction vector.
/// This function also checks for the edge case where the x and y components
/// are both zero. In that case, the function returns `None`.
let xyLength (Length.Length x: Length<'Unit>) (Length.Length y: Length<'Unit>) : Direction2D<'Coordinates> option = xy x y

/// Create a direction vector from the x and y components. This function
/// doesn't perform either zero magnitude checks nor does it normalize the
/// input vectors. This function should only be used with input constants or
/// when you are sure that you aren't going to create a direction with an
/// invalid state.
let xyUnsafe (x: float) (y: float) : Direction2D<'Coordinates> =
    { Direction2D.X = x; Direction2D.Y = y }

// Create an angle counterclockwise from the positive X direction.
let fromAngle (angle: Angle) : Direction2D<'Coordiantes> = xyUnsafe (Angle.cos angle) (Angle.sin angle)


(* Constants *)

let positiveX () : Direction2D<'Coordinates> = xyUnsafe 1.0 0.0

let positiveY () : Direction2D<'Coordinates> = xyUnsafe 0. 1.
let negativeX () : Direction2D<'Coordinates> = xyUnsafe -1. 0.
let negativeY () : Direction2D<'Coordinates> = xyUnsafe 0. -1.
let x = positiveX
let y = positiveY


(* Modifiers *)

// Rotate a direction by 90 degrees counterclockwise.
let rotateCounterclockwise (direction: Direction2D<'Coordinates>) : Direction2D<'Coordinates> =
    xyUnsafe -direction.Y direction.X

let placeIn
    (reference: Frame2D<'Unit, 'Coordinates>)
    (direction: Direction2D<'Coordinates>)
    : Direction2D<'Coordiantes> =
    let dx = reference.XDirection
    let dy = reference.YDirection

    { X = direction.X * dx.X + direction.Y * dy.X
      Y = direction.X * dx.Y + direction.Y * dy.Y }
