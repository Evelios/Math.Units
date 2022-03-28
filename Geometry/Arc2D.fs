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

/// Construct an arc with the given center point, radius, start angle and swept
let withSweptAngle
    (center: Point2D<'Unit, 'Coordinates>)
    (radius: Length<'Unit>)
    (startAngle: Angle)
    (sweptAngle: Angle)
    : Arc2D<'Unit, 'Coordinates> =
    let x0 = center.X
    let y0 = center.Y
    let startX = x0 - (radius * Angle.cos startAngle)
    let startY = y0 - (radius * Angle.sin startAngle)

    { StartPoint = Point2D.xy startX startY
      SweptAngle = sweptAngle
      XDirection = Direction2D.fromAngle (startAngle + Angle.halfPi)
      SignedLength = (Length.abs radius) * sweptAngle }

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

/// Get the radius of an arc.
let radius (arc: Arc2D<'Unit, 'Coordinates>) : Length<'Unit> = arc.SignedLength / arc.SweptAngle

/// Get the swept angle of an arc. The result will be positive for a
let sweptAngle (arc: Arc2D<'Unit, 'Coordinates>) : Angle = arc.SweptAngle

/// Get the point along an arc at a given parameter value.
let pointOn (arc: Arc2D<'Unit, 'Coordinates>) (parameterValue: float) : Point2D<'Unit, 'Coordinates> =
    let x0 = arc.StartPoint.X
    let y0 = arc.StartPoint.Y
    let dx = arc.XDirection.X
    let dy = arc.XDirection.Y
    let arcSignedLength = arc.SignedLength
    let arcSweptAngle = arc.SweptAngle

    if arcSweptAngle = Angle.zero then
        let distance = parameterValue * arcSignedLength
        let px = x0 + (distance * dx)
        let py = y0 + (distance * dy)
        Point2D.xy px py

    else
        let theta = parameterValue * arcSweptAngle
        let arcRadius = arcSignedLength / arcSweptAngle
        let x = arcRadius * Angle.sin theta

        let y =
            if Angle.abs theta < Angle.halfPi then
                x * Angle.tan (0.5 * theta)

            else
                (1. - Angle.cos theta) * arcRadius

        let px = x0 + (dx * x + -dy * y)
        let py = y0 + (dy * x + dx * y)

        Point2D.xy px py

/// Get the start point of an arc.
let startPoint (arc: Arc2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = arc.StartPoint

let midpoint (arc: Arc2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = pointOn arc 0.5

let endPoint (arc: Arc2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = pointOn arc 1.0

/// Get a bounding box for a given arc.
let boundingBox (givenArc: Arc2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    let xDirection = givenArc.XDirection
    let theta = sweptAngle givenArc

    if Angle.abs theta < Angle.degrees 5. then
        let p1 = startPoint givenArc
        let p2 = endPoint givenArc

        let offset =
            Length.half (Point2D.distanceTo p1 p2)
            / (Angle.cos (Angle.half theta))

        let offsetPoint =
            p1 |> Point2D.translateIn xDirection offset

        BoundingBox2D.hull3 p1 p2 offsetPoint

    else
        let startAngle =
            Direction2D.toAngle xDirection - Angle.halfPi

        let endAngle = startAngle + theta
        let startCos = Angle.cos startAngle
        let endCos = Angle.cos endAngle
        let startSin = Angle.sin startAngle
        let endSin = Angle.sin endAngle
        let cosMin = min startCos endCos
        let cosMax = max startCos endCos
        let sinMin = min startSin endSin
        let sinMax = max startSin endSin

        let x0, y0 =
            Point2D.coordinates (centerPoint givenArc)

        let r = radius givenArc

        { MinX = x0 + (r * cosMin)
          MaxX = x0 + (r * cosMax)
          MinY = y0 + (r * sinMin)
          MaxY = y0 + (r * sinMax) }

/// Get the first derivative of an arc at a given parameter value.
let firstDerivative (arc: Arc2D<'Unit, 'Coordinates>) (parameterValue: float) : Vector2D<'Unit, 'Coordinates> =
    let startDerivative =
        Vector2D.withLength arc.SignedLength arc.XDirection

    startDerivative
    |> Vector2D.rotateBy (parameterValue * arc.SweptAngle)

// ---- Non-Degenerative ----


/// Attempt to construct a non-degenerate arc from a general `Arc2d`. If the arc
/// is in fact degenerate (consists of a single point), returns an `Err` with that
/// point.
let nondegenerate
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Result<Nondegenerate<'Unit, 'Coordinates>, Point2D<'Unit, 'Coordinates>> =

    if arc.SignedLength = Length.zero then
        Error(startPoint arc)

    else
        Ok arc

/// Convert a nondegenerate arc back to a general `Arc2d`.
let fromNondegenerate (arc: Nondegenerate<'Unit, 'Coordinates>) : Arc2D<'Unit, 'Coordinates> = arc

///  Get the tangent direction to a nondegenerate arc at a given parameter
let tangentDirection (arc: Arc2D<'Unit, 'Coordinates>) (parameterValue: float) : Direction2D<'Coordinates> =
    arc.XDirection
    |> Direction2D.rotateBy (parameterValue * arc.SweptAngle)

/// Get both the point and tangent direction of a nondegenerate arc at a given
/// parameter value.
let sample
    (nondegenerateArc: Nondegenerate<'Unit, 'Coordinates>)
    (parameterValue: float)
    : Point2D<'Unit, 'Coordinates> * Direction2D<'Coordinates> =
    (pointOn (fromNondegenerate nondegenerateArc) parameterValue, tangentDirection nondegenerateArc parameterValue)

/// Reverse the direction of an arc, so that the start point becomes the end
/// point and vice versa.
let reverse (arc: Arc2D<'Unit, 'Coordinates>) : Arc2D<'Unit, 'Coordinates> =
    { StartPoint = endPoint arc
      SweptAngle = -arc.SweptAngle
      SignedLength = -arc.SignedLength
      XDirection =
          arc.XDirection
          |> Direction2D.rotateBy arc.SweptAngle }

/// Scale an arc about a given point by a given scale.
let scaleAbout
    (point: Point2D<'Unit, 'Coordinates>)
    (scale: float)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    { StartPoint = Point2D.scaleAbout point scale arc.StartPoint
      SweptAngle = arc.SweptAngle
      SignedLength = (abs scale) * arc.SignedLength
      XDirection =
          if scale >= 0. then
              arc.XDirection

          else
              Direction2D.reverse arc.XDirection }

/// Rotate an arc around a given point by a given angle.
let rotateAround
    (point: Point2D<'Unit, 'Coordinates>)
    (angle: Angle)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    { StartPoint = Point2D.rotateAround point angle arc.StartPoint
      SweptAngle = arc.SweptAngle
      SignedLength = arc.SignedLength
      XDirection = Direction2D.rotateBy angle arc.XDirection }


/// Translate an arc by a given displacement.
let translateBy
    (displacement: Vector2D<'Unit, 'Coordinates>)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    { StartPoint = Point2D.translateBy displacement arc.StartPoint
      SweptAngle = arc.SweptAngle
      SignedLength = arc.SignedLength
      XDirection = arc.XDirection }


/// Translate an arc in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Length<'Unit>)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    translateBy (Vector2D.withLength distance direction) arc


/// Mirror an arc across a given axis. This negates the sign of the arc's
/// swept angle.
let mirrorAcross (axis: Axis2D<'Unit, 'Coordinates>) (arc: Arc2D<'Unit, 'Coordinates>) : Arc2D<'Unit, 'Coordinates> =
    { StartPoint = Point2D.mirrorAcross axis arc.StartPoint
      SweptAngle = -arc.SweptAngle
      SignedLength = -arc.SignedLength
      XDirection = Direction2D.reverse (Direction2D.mirrorAcross axis arc.XDirection) }


/// Take an arc defined in global coordinates, and return it expressed in local
/// coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    if Frame2D.isRightHanded frame then
        { StartPoint = Point2D.relativeTo frame arc.StartPoint
          SweptAngle = arc.SweptAngle
          SignedLength = arc.SignedLength
          XDirection = Direction2D.relativeTo frame arc.XDirection }

    else
        { StartPoint = Point2D.relativeTo frame arc.StartPoint
          SweptAngle = -arc.SweptAngle
          SignedLength = -arc.SignedLength
          XDirection = Direction2D.reverse (Direction2D.relativeTo frame arc.XDirection) }


/// Take an arc considered to be defined in local coordinates relative to a
/// given reference frame, and return that arc expressed in global coordinates.
let placeIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (arc: Arc2D<'Unit, 'Coordinates>)
    : Arc2D<'Unit, 'Coordinates> =

    if Frame2D.isRightHanded frame then
        { StartPoint = Point2D.placeIn frame arc.StartPoint
          SweptAngle = arc.SweptAngle
          SignedLength = arc.SignedLength
          XDirection = Direction2D.placeIn frame arc.XDirection }

    else
        { StartPoint = Point2D.placeIn frame arc.StartPoint
          SweptAngle = arc.SweptAngle
          SignedLength = arc.SignedLength
          XDirection = Direction2D.reverse (Direction2D.placeIn frame arc.XDirection) }
