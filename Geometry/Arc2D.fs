[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Arc2D


// ---- Builders ----

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

/// Attempt to construct an arc that starts at the first given point, passes
/// through the second given point and ends at the third given point:
let throughPoints
    (first: Point2D<'Unit, 'Coordinates>)
    (second: Point2D<'Unit, 'Coordinates>)
    (third: Point2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> option =
    match Point2D.circumcenter first second third with
    | None -> None
    | Some circumcenter ->
        let firstVector = Vector2D.from circumcenter first
        let secondVector = Vector2D.from circumcenter second
        let thirdVector = Vector2D.from circumcenter third

        match (Vector2D.direction firstVector), (Vector2D.direction secondVector), (Vector2D.direction thirdVector) with
        | Some firstDirection, Some secondDirection, Some thirdDirection ->
            let partial =
                Direction2D.angleFrom firstDirection secondDirection

            let full =
                Direction2D.angleFrom firstDirection thirdDirection

            let computedSweptAngle =
                if partial >= Angle.zero && full >= partial then
                    full
                else if partial <= Angle.zero && full <= partial then
                    full
                else if full >= Angle.zero then
                    full - Angle.twoPi
                else
                    full + Angle.twoPi

            first
            |> sweptAround circumcenter computedSweptAngle
            |> Some

        | _ -> None

let withRadius
    (radius: Length<'Unit>)
    (sweptAngle: SweptAngle)
    (startPoint: Point2D<'Unit, 'Coordinates>)
    (endPoint: Point2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> option =

    let chord = LineSegment2D.from startPoint endPoint
    let squaredRadius = Length.square radius

    let squaredHalfLength =
        LineSegment2D.length chord
        |> (*) 0.5
        |> Length.square

    if squaredRadius < squaredHalfLength then
        None

    else
        match LineSegment2D.perpendicularDirection chord with
        | None -> None
        | Some offsetDirection ->
            let offsetMagnitude =
                Length.sqrt (squaredRadius - squaredHalfLength)

            let offsetDistance =
                match sweptAngle with
                | SmallPositive -> offsetMagnitude
                | SmallNegative -> -offsetMagnitude
                | LargeNegative -> offsetMagnitude
                | LargePositive -> -offsetMagnitude

            let computedCenterPoint =
                LineSegment2D.midpoint chord
                |> Point2D.translateIn offsetDirection offsetDistance

            let halfLength = Length.sqrt squaredHalfLength
            let shortAngle = 2. * Angle.asin (halfLength / radius)

            let sweptAngleInRadians =
                match sweptAngle with
                | SmallPositive -> shortAngle
                | SmallNegative -> -shortAngle
                | LargePositive -> Angle.twoPi - shortAngle
                | LargeNegative -> shortAngle - Angle.twoPi

            startPoint
            |> sweptAround computedCenterPoint sweptAngleInRadians
            |> Some


// ---- Accessors ----

// Get the center point of an arc.
let centerPoint (arc: Arc2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
    let x0 = arc.StartPoint.X
    let y0 = arc.StartPoint.Y
    let dx = arc.XDirection.X
    let dy = arc.XDirection.Y

    let r =
        arc.SignedLength
        / (Angle.inRadians arc.SweptAngle)

    let cx = x0 - (r * dy)
    let cy = y0 + (r * dx)

    Point2D.xy cx cy
