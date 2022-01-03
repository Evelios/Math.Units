namespace Geometry

module Boolean2D =
    let boundingBoxAndLine bbox line =
        match Intersection2D.boundingBoxAndLine bbox line with
        | [ first; second ] -> Some(LineSegment2D.from first second)
        | _ -> None
