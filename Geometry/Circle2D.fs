[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Circle2D

open System

// ---- Builders ----

let atPoint (center: Point2D<'Unit, 'Coordinates>) (radius: Length<'Unit>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let withRadius (radius: Length<'Unit>) (center: Point2D<'Unit, 'Coordinates>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let atOrigin radius = atPoint Point2D.origin radius

let throughPoints
    (p1: Point2D<'Unit, 'Coordinates>)
    (p2: Point2D<'Unit, 'Coordinates>)
    (p3: Point2D<'Unit, 'Coordinates>)
    =
    Point2D.circumcenter p1 p2 p3
    |> Option.map
        (fun p0 ->
            let r1 = Point2D.distanceTo p0 p1
            let r2 = Point2D.distanceTo p0 p2
            let r3 = Point2D.distanceTo p0 p3
            let r = (r1 + r2 + r3) * (1. / 3.)
            withRadius r p0)

let sweptAround (centerPoint: Point2D<'Units, 'Coordinates>) (point: Point2D<'Units, 'Coordinates>) =
    withRadius (Point2D.distanceTo centerPoint point) centerPoint

// ---- Accessors ----

let centerPoint (circle: Circle2D<'Unit, 'Coordinates>) : Point2D<'Unit, 'Coordinates> = circle.Center

let radius (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = circle.Radius

let diameter (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = circle.Radius * 2.

let area (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    2. * Math.PI * (Length.squared circle.Radius)

let circumference (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = 2. * Math.PI * circle.Radius

let toArc (circle: Circle2D<'Unit, 'Coordinates>) : Arc2D<'Unit, 'Coordinates> =
    let startX = circle.Center.X + circle.Radius
    let startY = circle.Center.X

    { StartPoint = Point2D.xy startX startY
      XDirection = Direction2D.y
      SweptAngle = Angle.radians (2. * Math.PI)
      SignedLength = (2. * Math.PI) * circle.Radius }


let boundingBox (circle: Circle2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    BoundingBox2D.fromExtrema
        { MinX = circle.Center.X - circle.Radius
          MaxX = circle.Center.X + circle.Radius
          MinY = circle.Center.Y - circle.Radius
          MaxY = circle.Center.Y + circle.Radius }

//  ---- Modifiers ----
//// Scale a circle about a given point by a given scale.
let scaleAbout
    (point: Point2D<'Unit, 'Coordinates>)
    (scale: float)
    (circle: Circle2D<'Unit, 'Coordinates>)
    : Circle2D<'Unit, 'Coordinates> =

    withRadius ((abs scale) * circle.Radius) (Point2D.scaleAbout point scale circle.Center)

/// Translate a circle by a given displacement.
let translateBy
    (displacement: Vector2D<'Unit, 'Coordinates>)
    (circle: Circle2D<'Unit, 'Coordinates>)
    : Circle2D<'Unit, 'Coordinates> =
        
    withRadius circle.Radius (Point2D.translateBy displacement circle.Center)


/// Translate a circle in a given direction by a given distance.
let translateIn
    (direction: Direction2D<'Coordinates>)
    (distance: Length<'Unit>)
    (circle: Circle2D<'Unit, 'Coordinates>)
    : Circle2D<'Unit, 'Coordinates> =
        
    translateBy (Vector2D.withLength distance direction) circle


/// Mirror a circle across a given axis.
let mirrorAcross (axis: Axis2D<'Unit, 'Coordinates>) (circle: Circle2D<'Unit, 'Coordinates>) =
    withRadius circle.Radius (Point2D.mirrorAcross axis circle.Center)


/// Take a circle defined in global coordinates, and return it expressed in
/// local coordinates relative to a given reference frame.
let relativeTo
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (circle: Circle2D<'Unit, 'Coordinates>)
    : Circle2D<'Unit, 'Coordinates> =
        
    withRadius circle.Radius (Point2D.relativeTo frame circle.Center)


/// Take a circle considered to be defined in local coordinates relative to a
/// given reference frame, and return that circle expressed in global coordinates.
let placeIn
    (frame: Frame2D<'Unit, 'Coordinates, 'Defines>)
    (circle: Circle2D<'Unit, 'Coordinates>)
    : Circle2D<'Unit, 'Coordinates> =
        
    withRadius circle.Radius (Point2D.placeIn frame circle.Center)

// ---- Queries ----

/// Test if a circle point is contained within a circle. A point is considered
/// to be contained if the point lies on the edge of the circle.
let containsPoint (point: Point2D<'Unit, 'Coordinates>) (circle: Circle2D<'Unit, 'Coordinates>) : bool =
    Point2D.distanceSquaredTo point circle.Center
    <= Length.squared circle.Radius

/// Check if a circle intersects with a given bounding box. This function will
/// return true if the circle intersects the edges of the bounding box _or_ is fully
/// contained within the bounding box.
let intersectsBoundingBox (box: BoundingBox2D<'Unit, 'Coordinates>) (circle: Circle2D<'Unit, 'Coordinates>) : bool =
    let deltaX =
        circle.Center.X
        - Length.max box.MinX (Length.min circle.Center.X box.MaxX)

    let deltaY =
        circle.Center.Y
        - (Length.max box.MinY (Length.min circle.Center.Y box.MaxY))

    Length.squared deltaX + (Length.squared deltaY)
    <= (Length.squared circle.Radius)
