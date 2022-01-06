namespace Geometry

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Frame2D =
    (* Builders *)
    let atOrigin : Frame2D<'Length, 'Coordinates> =
        { Origin = { x = 0.; y = 0. }
          XDirection = Direction2D.x
          YDirection = Direction2D.y }

    let atPoint (point: Point2D<'Length, 'Coordinates>) : Frame2D<'Length, 'Coordinates> =
        { Origin = point
          XDirection = Direction2D.x
          YDirection = Direction2D.y }

    let withXDirection xDirection origin =
        { Origin = origin
          XDirection = xDirection
          YDirection = Direction2D.rotateCounterclockwise xDirection }

    let withAngle (angle: Angle) (origin: Point2D<'Length, 'Coordinates>) : Frame2D<'Length, 'Coordinates> =
        withXDirection (Direction2D.fromAngle angle) origin

    (* Modifiers *)

    let placeIn
        (reference: Frame2D<'Length, 'Coordinates>)
        (frame: Frame2D<'Length, 'Coordinates>)
        : Frame2D<'Length, 'Coordinates> =
        { Origin = Point2D.placeIn reference frame.Origin
          XDirection = Direction2D.placeIn reference frame.XDirection
          YDirection = Direction2D.placeIn reference frame.YDirection }
