[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Frame2D

(* Builders *)
let atOrigin () : Frame2D<'Unit, 'Coordinates> =
    { Origin = Point2D.origin ()
      XDirection = Direction2D.x ()
      YDirection = Direction2D.y () }

let atPoint (point: Point2D<'Unit, 'Coordinates>) : Frame2D<'Unit, 'Coordinates> =
    { Origin = point
      XDirection = Direction2D.x ()
      YDirection = Direction2D.y () }

let withXDirection xDirection origin =
    { Origin = origin
      XDirection = xDirection
      YDirection = Direction2D.rotateCounterclockwise xDirection }

let withAngle (angle: Angle) (origin: Point2D<'Unit, 'Coordinates>) : Frame2D<'Unit, 'Coordinates> =
    withXDirection (Direction2D.fromAngle angle) origin

(* Modifiers *)

let placeIn
    (reference: Frame2D<'Unit, 'Coordinates>)
    (frame: Frame2D<'Unit, 'Coordinates>)
    : Frame2D<'Unit, 'Coordinates> =
    { Origin = Point2D.placeIn reference frame.Origin
      XDirection = Direction2D.placeIn reference frame.XDirection
      YDirection = Direction2D.placeIn reference frame.YDirection }
