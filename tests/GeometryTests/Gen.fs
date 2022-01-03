namespace GeometryTests

module Gen =
    open System
    open FsCheck

    open Geometry
    open Utilities
    open Utilities.Extensions

    let angle =
        Gen.map (fun angle -> Angle.inRadians (angle * 1.<rad>)) (Gen.floatBetween 0. (Math.PI / 2.))

    let vector2D = Gen.map2 Vector2D.xy Gen.float Gen.float

    let vector2DWithinRadius radius =
        Gen.map2 Vector2D.ofPolar (Gen.floatBetween 0. radius) angle

    let twoCloseVector2D =
        Gen.map2 (fun first offset -> (first, first + offset)) vector2D (vector2DWithinRadius Epsilon)

    let point2D = Gen.map2 Point2D.xy Gen.float Gen.float

    let point2DWithinOffset radius (point: Point2D) =
        Gen.map (fun offset -> point + offset) (vector2DWithinRadius radius)

    /// Generate two points that are within Epsilon of each other
    let twoClosePoint2D =
        Gen.map2 (fun first offset -> (first, first + offset)) point2D (vector2DWithinRadius Epsilon)

    let line2D =
        Gen.map2 Tuple2.pair point2D point2D
        |> Gen.filter (fun (p1, p2) -> p1 <> p2)
        |> Gen.map (Tuple2.map Line2D.through)

    let lineSegment2D =
        Gen.map2 Tuple2.pair point2D point2D
        |> Gen.filter (fun (p1, p2) -> p1 <> p2)
        |> Gen.map (Tuple2.map LineSegment2D.from)

    let boundingBox2D =
        Gen.map2 BoundingBox2D.from point2D point2D

    let point2DInBoundingBox2D (bbox: BoundingBox2D) =
        Gen.map2 Point2D.xy (Gen.floatBetween bbox.MinX bbox.MaxX) (Gen.floatBetween bbox.MinY bbox.MaxY)

    let lineSegment2DInBoundingBox2D (bbox: BoundingBox2D) =
        Gen.two (point2DInBoundingBox2D bbox)
        |> Gen.where (fun (a, b) -> a <> b)
        |> Gen.map (Tuple2.map LineSegment2D.from)

    type ArbGeometry =
        static member Vector2D() = Arb.fromGen vector2D
        static member Point2D() = Arb.fromGen point2D
        static member Line2D() = Arb.fromGen line2D
        static member LineSegment2D() = Arb.fromGen line2D
        static member BoundingBox2D() = Arb.fromGen line2D

        static member Register() = Arb.register<ArbGeometry> () |> ignore
