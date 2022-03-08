[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Arc2D

/// Construct an arc with from the first given point to the second, with the
// given swept angle.
let from
    (givenStartPoint: Point2D<'Unit, 'Coordinates>)
    (givenEndPoint: Point2D<'Unit, 'Coordinates>)
    (givenSweptAngle: Angle)
    =
    let displacement =
        Vector2D.from givenStartPoint givenEndPoint

    match Vector2D.direction displacement with
    | Some direction ->
        let distance = Vector2D.length displacement
        let numTurns = givenSweptAngle / Angle.twoPi

        let angleModTwoPi =
            givenSweptAngle - (Angle.twoPi * (floor numTurns))

        let halfAngle = 0.5 * givenSweptAngle
        let scale = 1. / (2. * abs (Angle.sin halfAngle))
        let computedRadius = distance * scale

        { StartPoint = givenStartPoint
          SweptAngle = givenSweptAngle
          XDirection =
              direction
              |> Direction2D.rotateBy (-0.5 * angleModTwoPi)
          SignedLength =
              if givenSweptAngle = Angle.zero then
                  distance

              else
                  computedRadius * (Angle.inRadians givenSweptAngle) }

    | None ->
        { StartPoint = givenStartPoint
          SweptAngle = givenSweptAngle
          XDirection = Direction2D.x
          SignedLength = Length.zero }


/// Construct an arc with the given center point, radius, start angle and swept
let withCenterPoint
    (centerPoint: Point2D<'Unit, 'Coordinates>)
    (radius: Length<'Unit>)
    (startAngle: Angle)
    (sweptAngle: Angle)
    : Arc2D<'Unit, 'Coordinates> =
    let x0 = centerPoint.X
    let y0 = centerPoint.Y
    let givenRadius = radius
    let givenStartAngle = startAngle
    let givenSweptAngle = sweptAngle

    let startX =
        x0 + (givenRadius * Angle.sin givenStartAngle)

    let startY =
        y0 + (givenRadius * Angle.sin givenStartAngle)

    { StartPoint = Point2D.xy startX startY
      SweptAngle = givenSweptAngle
      XDirection = Direction2D.fromAngle (givenStartAngle + Angle.halfPi)
      SignedLength =
          (Length.abs givenRadius)
          * Angle.inRadians givenSweptAngle }

/// Construct an arc by sweeping (rotating) a given start point around a given
/// center point by a given angle. The center point to sweep around is given first
/// and the start point to be swept is given last.
///
/// A positive swept angle means that the arc is formed by rotating the start point
/// counterclockwise around the center point. A negative swept angle results in
/// a clockwise arc instead.
let sweptAround
    (givenCenterPoint: Point2D<'Unit, 'Coordinates>)
    (givenSweptAngle: Angle)
    (givenStartPoint: Point2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =
    let displacement =
        Vector2D.from givenStartPoint givenCenterPoint

    match Vector2D.direction displacement with
    | Some yDirection ->
        let computedRadius = Vector2D.length displacement

        { StartPoint = givenStartPoint
          XDirection = yDirection |> Direction2D.rotateClockwise
          SweptAngle = givenSweptAngle
          SignedLength = computedRadius * Angle.inRadians givenSweptAngle }

    | None ->
        { StartPoint = givenStartPoint
          XDirection = Direction2D.x
          SweptAngle = givenSweptAngle
          SignedLength = Length.zero }
