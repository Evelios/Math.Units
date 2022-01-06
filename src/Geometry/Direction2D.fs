namespace Geometry

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Direction2D =
    (* Constants *)

    let xy x y =
        { Direction2D.X = x; Direction2D.Y = y }

    let positiveX = xy 1. 0.
    let positiveY = xy 0. 1.
    let negativeX = xy -1. 0.
    let negativeY = xy 0. -1.
    let x = positiveX
    let y = positiveY

    (* Builders *)

    // Create an angle counterclockwise from the positive X direction.
    let fromAngle (angle: Angle) = xy (Angle.cos angle) (Angle.sin angle)

    (* Modifiers *)

    // Rotate a direction by 90 degrees counterclockwise.
    let rotateCounterclockwise (direction: Direction2D) : Direction2D = xy -direction.Y direction.X

    let placeIn (reference: Frame2D<'Length, 'Coordinates>) (direction: Direction2D) : Direction2D =
        let dx = reference.XDirection
        let dy = reference.YDirection

        { X = direction.X * dx.X + direction.Y * dy.X
          Y = direction.X * dx.Y + direction.Y * dy.Y }
