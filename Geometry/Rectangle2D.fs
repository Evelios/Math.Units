[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Rectangle2D

open Units

let private axisAligned
    (x1: Quantity<'Units>)
    (y1: Quantity<'Units>)
    (x2: Quantity<'Units>)
    (y2: Quantity<'Units>)
    : Rectangle2D<'Units, 'Coordinates> =

    let computedCenterPoint =
        Point2D.xy (Length.midpoint x1 x2) (Length.midpoint y1 y2)

    let computedXDirection =
        if x2 >= x1 then
            Direction2D.positiveX
        else
            Direction2D.negativeX

    let computedYDirection =
        if y2 >= y1 then
            Direction2D.positiveY
        else
            Direction2D.negativeY

    let computedAxes =
        { Origin = computedCenterPoint
          XDirection = computedXDirection
          YDirection = computedYDirection }

    let computedDimensions =
        Size2D.create (Length.abs (x2 - x1)) (Length.abs (y2 - y1))

    { Axes = computedAxes
      Dimensions = computedDimensions }

/// Construct an axis-aligned rectangle given the X and Y coordinates of two
/// diagonally opposite vertices. The X and Y directions of the resulting rectangle
/// are determined by the order of the given values:
///   - If `x1 <= x2`, then the rectangle's X direction will be
///     `Direction2D.positiveX`, otherwise it will be `Direction2D.negativeX`
///   - If `y1 <= y2`, then the rectangle's Y direction will be
///     `Direction2D.positiveY`, otherwise it will be `Direction2D.negativeY`
/// Therefore, something like
///     Rectangle2D.from
///         (Pixels.pixels 0)
///         (Pixels.pixels 300)
///         (Pixels.pixels 500)
///         (Pixels.pixels 0)
/// would have its X direction equal to `Direction2D.positiveX` and its Y direction
/// equal to `Direction2D.negativeY`.
/// -}
let from
    (x1: Quantity<'Units>)
    (y1: Quantity<'Units>)
    (x2: Quantity<'Units>)
    (y2: Quantity<'Units>)
    : Rectangle2D<'Units, 'Coordinates> =
    axisAligned x1 y1 x2 y2

/// Construct an axis-aligned rectangle stretching from one point to another;
///     Rectangle2D.from p1 p2
/// is equivalent to
///     Rectangle2D.with p1.X p1.Y p2.X p2.Y
/// and so the same logic about the resulting rectangle's X and Y directions
/// applies (see above for details). Therefore, assuming a Y-up coordinate system,
/// something like
///     Rectangle2D.from lowerLeftPoint upperRightPoint
/// would have positive X and Y directions, while
///     Rectangle2D.from upperLeftPoint lowerRightPoint
/// would have a positive X direction but a negative Y direction.
let fromPoints
    (p1: Point2D<'Units, 'Coordinates>)
    (p2: Point2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =
    axisAligned p1.X p1.Y p2.X p2.Y

// Construct a rectangle with the given overall dimensions (width/height) and
// angle, centered on the given point. For an axis-aligned rectangle you can pass
// `Angle.degrees 0` (or equivalently `Angle.radians 0` or even `Quantity.zero`) as
// the second argument:
//     rectangle =
//         Rectangle2D.withDimensions
//             (Size.create (Pixels.pixels 200) (Pixels.pixels 100))
//             (Angle.degrees 0)
//             (Point2D.pixels 400 300)
//     Rectangle2D.vertices rectangle
//     --> [ Point2D.pixels 300 250
//     --> , Point2D.pixels 500 250
//     --> , Point2D.pixels 500 350
//     --> , Point2D.pixels 300 350
//     --> ]
// Passing a non-zero angle lets you control the orientation of a rectangle; to
// make a diamond shape you might do something like
//     diamond =
//         Rectangle2D.withDimensions
//             (Size.create (Length.meters 2) (Length.meters 2))
//             (Angle.degrees 45)
//             Point2D.origin
//     Rectangle2D.vertices diamond
//     --> [ Point2D.meters 0 -1.4142
//     --> , Point2D.meters 1.4142 0
//     --> , Point2D.meters 0 1.4142
//     --> , Point2D.meters -1.4142 0
//     --> ]
let withDimensions
    (size: Size2D<'Units>)
    (angle: Angle)
    (center: Point2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    { Axes = Frame2D.withAngle angle center
      Dimensions = size }

/// Construct a frame centered on the given axes, with the given overall
/// dimensions;
///     Rectangle2D.withDimensions dimensions angle centerPoint
/// could be written as
///     Rectangle2D.centeredOn
///         (Frame2D.withAngle angle centerPoint)
///         dimensions
let centeredOn
    (givenAxes: Frame2D<'Units, 'Coordinates, 'Defines>)
    (dimensions: Size2D<'Units>)
    : Rectangle2D<'Units, 'Coordinates> =

    { Axes = Frame2D.copy givenAxes
      Dimensions = dimensions }

/// Construct a rectangle with the given X axis and overall dimensions. The
/// rectangle will be centered on the axis' origin point.
let withXAxis (axis: Axis2D<'Units, 'Coordinates>) (dimensions: Size2D<'Units>) : Rectangle2D<'Units, 'Coordinates> =
    centeredOn (Frame2D.fromXAxis axis) dimensions


/// Construct a rectangle with the given Y axis and overall dimensions. The
/// rectangle will be centered on the axis' origin point.
let withYAxis (axis: Axis2D<'Units, 'Coordinates>) (dimensions: Size2D<'Units>) : Rectangle2D<'Units, 'Coordinates> =
    centeredOn (Frame2D.fromYAxis axis) dimensions

/// Convert a `BoundingBox2D` to the equivalent axis-aligned `Rectangle2D`.
let fromBoundingBox (box: BoundingBox2D<'Units, 'Coordinates>) : Rectangle2D<'Units, 'Coordinates> =
    axisAligned box.MinX box.MinY box.MaxX box.MaxY


// ---- Accessors ----

/// Get the central axes of a rectangle as a `Frame2D`:
///     rectangle =
///         Rectangle2D.with
///             (Length.meters 2)
///             (Length.meters 5)
///             (Length.meters 1)
///             (Length.meters 3)
///     Rectangle2D.axes rectangle
///     --> Frame2D.atPoint (Point2D.meters 3.5 2)
/// The origin point of the frame will be the center point of the rectangle.
let axes (rectangle: Rectangle2D<'Units, 'Coordinates>) : Frame2D<'Units, 'Coordinates, 'Defines> =
    Frame2D.copy rectangle.Axes


/// Get the X axis of a rectangle.
let xAxis (rectangle: Rectangle2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> = Frame2D.xAxis (axes rectangle)


/// Get the Y axis of a rectangle.
let yAxis (rectangle: Rectangle2D<'Units, 'Coordinates>) : Axis2D<'Units, 'Coordinates> = Frame2D.yAxis (axes rectangle)

/// Get the center point of a rectangle.
let centerPoint (rectangle: Rectangle2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> =
    Frame2D.originPoint (axes rectangle)


/// Get the overall dimensions (width and height) of a rectangle:
///     rectangle =
///         Rectangle2D.with
///             { x1 = Length.meters 2
///             , x2 = Length.meters 5
///             , y1 = Length.meters 1
///             , y2 = Length.meters 3
///             }
///     Rectangle2D.dimensions rectangle
///     --> ( Length.meters 3, Length.meters 2 )
let dimensions (rectangle: Rectangle2D<'Units, 'Coordinates>) : Size2D<'Units> = rectangle.Dimensions


/// Get the area of a rectangle.
let area (rectangle: Rectangle2D<'Units, 'Coordinates>) : Quantity<'Units Squared> =
    rectangle.Dimensions.Width
    * rectangle.Dimensions.Height

/// Get the vertices of a rectangle as a list. The vertices will be returned
/// in counterclockwise order if the rectangle's axes are right-handed, and
/// clockwise order if the axes are left-handed.
let vertices (rectangle: Rectangle2D<'Units, 'Coordinates>) : Point2D<'Units, 'Coordinates> list =
    let localFrame = axes rectangle
    let x = Length.half rectangle.Dimensions.Width
    let y = Length.half rectangle.Dimensions.Width

    [ Point2D.xyIn localFrame -x -y
      Point2D.xyIn localFrame x -y
      Point2D.xyIn localFrame x y
      Point2D.xyIn localFrame -x y ]


/// Convert a rectangle to a [`Polygon2D`](Polygon2D#Polygon2D).
let toPolygon (rectangle: Rectangle2D<'Units, 'Coordinates>) : Polygon2D<'Units, 'Coordinates> =
    { OuterLoop = (vertices rectangle)
      InnerLoops = [] }


/// Check if a rectangle contains a given point.
let contains (point: Point2D<'Units, 'Coordinates>) (rectangle: Rectangle2D<'Units, 'Coordinates>) : bool =
    let localFrame = axes rectangle
    let x = Point2D.xCoordinateIn localFrame point
    let y = Point2D.yCoordinateIn localFrame point

    Length.abs x
    <= Length.half rectangle.Dimensions.Width
    && Length.abs y
       <= Length.half rectangle.Dimensions.Height

/// Get the edges of a rectangle as a list. The edges will be returned
/// in counterclockwise order if the rectangle's axes are right-handed, and
/// clockwise order if the axes are left-handed.
let edges (rectangle: Rectangle2D<'Units, 'Coordinates>) : LineSegment2D<'Units, 'Coordinates> list =
    let localFrame = axes rectangle
    let x = Length.half rectangle.Dimensions.Width
    let y = Length.half rectangle.Dimensions.Height
    let p1 = Point2D.xyIn localFrame -x -y
    let p2 = Point2D.xyIn localFrame x -y
    let p3 = Point2D.xyIn localFrame x y
    let p4 = Point2D.xyIn localFrame -x y

    [ LineSegment2D.from p1 p2
      LineSegment2D.from p2 p3
      LineSegment2D.from p3 p4
      LineSegment2D.from p4 p1 ]

/// Get the minimal bounding box containing a given rectangle. This will have
/// exactly the same shape and size as the rectangle itself if the rectangle is
/// axis-aligned, but will be larger than the rectangle if the rectangle is at an
/// angle.
///     square =
///         Rectangle2D.with
///             { x1 = Length.meters 0
///             , x2 = Length.meters 1
///             , y1 = Length.meters 0
///             , y2 = Length.meters 1
///             }
///     diamond =
///         square
///             |> Rectangle2D.rotateAround Point2D.origin
///                 (Angle.degrees 45)
///     Rectangle2D.boundingBox diamond
///     --> BoundingBox2D.from
///     -->     (Point2D.meters -0.7071 0)
///     -->     (Point2D.meters 0.7071 1.4142)
let boundingBox (rectangle: Rectangle2D<'Units, 'Coordinates>) : BoundingBox2D<'Units, 'Coordinates> =
    let frame = axes rectangle
    let p0 = Frame2D.originPoint frame
    let i = Frame2D.xDirection frame
    let ix = abs i.X
    let iy = abs i.Y
    let j = Frame2D.yDirection frame
    let jx = abs j.X
    let jy = abs j.Y
    let halfWidth = rectangle.Dimensions.Width / 2.
    let halfHeight = rectangle.Dimensions.Height / 2.
    let minX = p0.X - ix * halfWidth - jx * halfHeight
    let maxX = p0.X + ix * halfWidth + jx * halfHeight
    let minY = p0.Y - iy * halfWidth - jy * halfHeight
    let maxY = p0.Y + iy * halfWidth + jy * halfHeight

    { MinX = minX
      MaxX = maxX
      MinY = minY
      MaxY = maxY }


// ---- Modifiers ----

/// Scale a rectangle about a given point by a given scale. Note that scaling by
/// a negative value will flip the handedness of the rectangle's axes, and therefore
/// the order/direction of results from `Rectangle2D.vertices` and
/// `Rectangle2D.edges` will change.
let scaleAbout
    (point: Point2D<'Units, 'Coordinates>)
    (scale: float)
    (rectangle: Rectangle2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    let currentFrame = axes rectangle
    let currentXDirection = Frame2D.xDirection currentFrame
    let currentYDirection = Frame2D.yDirection currentFrame

    let newCenterPoint =
        Point2D.scaleAbout point scale (Frame2D.originPoint currentFrame)

    let newAxes =
        if scale >= 0. then
            { Origin = newCenterPoint
              XDirection = currentXDirection
              YDirection = currentYDirection }

        else
            { Origin = newCenterPoint
              XDirection = Direction2D.reverse currentXDirection
              YDirection = Direction2D.reverse currentYDirection }

    let newWidth =
        Length.abs (scale * rectangle.Dimensions.Width)

    let newHeight =
        Length.abs (scale * rectangle.Dimensions.Height)

    { Axes = newAxes
      Dimensions = Size2D.create newWidth newHeight }

/// Rotate a rectangle around a given point by a given angle.
let rotateAround
    (point: Point2D<'Units, 'Coordinates>)
    (angle: Angle)
    (rectangle: Rectangle2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    { Axes = Frame2D.rotateAround point angle (axes rectangle)
      Dimensions = dimensions rectangle }

/// Translate a rectangle by a given displacement.
let translateBy
    (displacement: Vector2D<'Units, 'Coordinates>)
    (rectangle: Rectangle2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    { Axes = Frame2D.translateBy displacement (axes rectangle)
      Dimensions = dimensions rectangle }

/// Translate a rectangle in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Quantity<'Units>)
    (rectangle: Rectangle2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    translateBy (Vector2D.withQuantity distance direction) rectangle

/// Mirror a rectangle across a given axis. Note that this will flip the
/// handedness of the rectangle's axes, and therefore the order/direction of results
/// from `Rectangle2D.vertices` and `Rectangle2D.edges` will change.
let mirrorAcross
    (axis: Axis2D<'Units, 'Coordinates>)
    (rectangle: Rectangle2D<'Units, 'Coordinates>)
    : Rectangle2D<'Units, 'Coordinates> =

    { Axes = Frame2D.mirrorAcross axis (axes rectangle)
      Dimensions = dimensions rectangle }

/// Take a rectangle considered to be defined in local coordinates relative to a
/// given reference frame, and return that rectangle expressed in global
/// coordinates.
let placeIn
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (rectangle: Rectangle2D<'Units, 'GlobalCoordinates>)
    : Rectangle2D<'Units, 'LocalCoordinates> =

    { Axes = Frame2D.placeIn frame (axes rectangle)
      Dimensions = dimensions rectangle }

/// Take a rectangle defined in global coordinates, and return it expressed
/// in local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Units, 'GlobalCoordinates, 'Defines>)
    (rectangle: Rectangle2D<'Units, 'GlobalCoordinates>)
    : Rectangle2D<'Units, 'LocalCoordinates> =

    { Axes = Frame2D.relativeTo frame (axes rectangle)
      Dimensions = dimensions rectangle }

/// Interpolate within a rectangle based on coordinates which range from 0 to 1.
/// For example, the four vertices of a given rectangle are
///     [ Rectangle2D.interpolate rectangle 0 0
///       Rectangle2D.interpolate rectangle 1 0
///       Rectangle2D.interpolate rectangle 1 1
///       Rectangle2D.interpolate rectangle 0 1
///     ]
/// and its center point is
///     Rectangle2D.interpolate rectangle 0.5 0.5
let interpolate (rectangle: Rectangle2D<'Units, 'Coordinates>) (u: float) (v: float) : Point2D<'Units, 'Coordinates> =
    Point2D.xyIn (axes rectangle) ((u - 0.5) * rectangle.Dimensions.Width) ((v - 0.5) * rectangle.Dimensions.Height)
