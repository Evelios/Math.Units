[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Geometry.Circle2D

open System

(* Builders *)

let atPoint (center: Point2D<'Unit, 'Coordinates>) (radius: Length<'Unit>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let withRadius (radius: Length<'Unit>) (center: Point2D<'Unit, 'Coordinates>) : Circle2D<'Unit, 'Coordinates> =
    { Center = center; Radius = radius }

let atOrigin radius = atPoint (Point2D.origin ()) radius


(* Accessors *)

let diameter (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = circle.Radius * 2.

let area (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit * 'Unit> =
    2. * Math.PI * (Length.square circle.Radius)

let circumference (circle: Circle2D<'Unit, 'Coordinates>) : Length<'Unit> = 2. * Math.PI * circle.Radius

let boundingBox (circle: Circle2D<'Unit, 'Coordinates>) : BoundingBox2D<'Unit, 'Coordinates> =
    BoundingBox2D.fromExtrema
        { MinX = circle.Center.X - circle.Radius
          MaxX = circle.Center.X + circle.Radius
          MinY = circle.Center.Y - circle.Radius
          MaxY = circle.Center.Y - circle.Radius }

(* Queries *)

let containsPoint (point: Point2D<'Unit, 'Coordinates>) (circle: Circle2D<'Unit, 'Coordinates>) : bool =
    Point2D.distanceSquaredTo point circle.Center <= Length.square circle.Radius
