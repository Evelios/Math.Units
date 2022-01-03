namespace Geometry

open System

type Circle2D = { Center: Point2D; Radius: float }


module Circle2D =
    (* Builders *)
    let atPoint (center: Point2D) (radius: float) : Circle2D = { Center = center; Radius = radius }
    let withRadius (radius: float) (center: Point2D) : Circle2D = { Center = center; Radius = radius }
    let atOrigin (radius: float) = atPoint Point2D.origin radius


    (* Accessors *)
    let diameter (circle: Circle2D) : float = circle.Radius * 2.
    let area (circle: Circle2D) : float = 2. * Math.PI * (circle.Radius ** 2.)
    let circumference (circle: Circle2D) : float = 2. * Math.PI * circle.Radius

    let boundingBox (circle: Circle2D) : BoundingBox2D =
        BoundingBox2D.fromExtrema
            { MinX = circle.Center.X - circle.Radius
              MaxX = circle.Center.X + circle.Radius
              MinY = circle.Center.Y - circle.Radius
              MaxY = circle.Center.Y - circle.Radius }
