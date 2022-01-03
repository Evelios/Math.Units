namespace GeometrySvg

open Geometry
open SharpVG

module Draw =
    let private point2DToPoint (point: Point2D) : Point =
        Point.create (Length.ofFloat point.X) (Length.ofFloat point.Y)

    [<Literal>]
    let strokeWidth = 3.

    [<Literal>]
    let pointSize = 5.

    let point (point: Point2D) =
        Circle.create (point2DToPoint point) (Length.ofFloat pointSize)
        |> Element.create

    let polygon (polygon: Polygon2D) =
        Seq.map point2DToPoint polygon.Points
        |> List.ofSeq
        |> Polygon.ofList
        |> Element.create
