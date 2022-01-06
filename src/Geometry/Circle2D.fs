namespace Geometry

open System

type Circle2D<'Length, 'Coordinates> =
    { Center: Point2D<'Length, 'Coordinates>
      Radius: float }


module Circle2D =
    (* Builders *)
    let atPoint (center: Point2D<'Length, 'Coordinates>) (radius: float) : Circle2D<'Length, 'Coordinates> =
        { Center = center; Radius = radius }

    let withRadius (radius: float) (center: Point2D<'Length, 'Coordinates>) : Circle2D<'Length, 'Coordinates> =
        { Center = center; Radius = radius }

    let atOrigin radius = atPoint (Point2D.origin ()) radius


    (* Accessors *)
    let diameter (circle: Circle2D<'Length, 'Coordinates>) : float = circle.Radius * 2.

    let area (circle: Circle2D<'Length, 'Coordinates>) : float = 2. * Math.PI * (circle.Radius ** 2.)
    let circumference (circle: Circle2D<'Length, 'Coordinates>) : float = 2. * Math.PI * circle.Radius

    let boundingBox (circle: Circle2D<'Length, 'Coordinates>) : BoundingBox2D<'Length, 'Coordinates> =
        BoundingBox2D.fromExtrema
            { MinX = circle.Center.X - circle.Radius
              MaxX = circle.Center.X + circle.Radius
              MinY = circle.Center.Y - circle.Radius
              MaxY = circle.Center.Y - circle.Radius }
