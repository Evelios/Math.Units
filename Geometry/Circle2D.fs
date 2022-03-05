[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Circle2D

open System

// ---- Builders ----

let atPoint (center: Point2D<'Unit, 'Coordinates>) (radius: Length<'Unit>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let withRadius (radius: Length<'Unit>) (center: Point2D<'Unit, 'Coordinates>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let atOrigin radius = atPoint (Point2D.origin ()) radius

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

let diameter (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = circle.Radius * 2.

let area (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    2. * Math.PI * (Length.square circle.Radius)

let circumference (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = 2. * Math.PI * circle.Radius

let boundingBox (circle: Circle2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    BoundingBox2D.fromExtrema
        { MinX = circle.Center.X - circle.Radius
          MaxX = circle.Center.X + circle.Radius
          MinY = circle.Center.Y - circle.Radius
          MaxY = circle.Center.Y + circle.Radius }

(* Queries *)

let containsPoint (point: Point2D<'Unit, 'Coordinates>) (circle: Circle2D<'Unit, 'Coordinates>) : bool =
    Point2D.distanceSquaredTo point circle.Center
    <= Length.square circle.Radius
