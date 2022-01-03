namespace Geometry

open FSharp.Json

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Point2D =
    (* Builders *)

    let xy (x: float) (y: float) : Point2D = { x = x; y = y }

    let rTheta (r: float) (theta: Angle) : Point2D =
        { x = r * Angle.cos theta
          y = r * Angle.sin theta }

    let ofPolar r a = xy (r * Angle.cos a) (r * Angle.sin a)

    let origin = xy 0. 0.

    (* Modifiers *)

    let toVector (point: Point2D) : Vector2D = Vector2D.xy point.x point.y

    let scale x y (point: Point2D) : Point2D = { x = point.x * x; y = point.y * y }

    let translate (v: Vector2D) (p: Point2D) = p + v

    let rotateAround (reference: Point2D) (angle: Angle) (point: Point2D) : Point2D =
        let c = Angle.cos angle
        let s = Angle.sin angle
        let deltaX = point.x - reference.x
        let deltaY = point.y - reference.y

        { x = reference.x + c * deltaX - s * deltaY
          y = reference.y + s * deltaX + c * deltaY }

    let placeIn (frame: Frame2D) (point: Point2D) : Point2D =
        let i = frame.XDirection
        let j = frame.YDirection

        { x = frame.Origin.x + point.X * i.X + point.Y * j.X
          y = frame.Origin.y + point.X * i.Y + point.Y * j.Y }

    (* Queries *)

    let distanceSquaredTo (p1: Point2D) (p2: Point2D) : float =
        let dx = (p1.x - p2.x)
        let dy = (p1.y - p2.y)
        dx * dx + dy * dy

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    let midpoint (p1: Point2D) (p2: Point2D) : Point2D =
        xy ((p1.x + p2.x) / 2.) ((p1.y + p2.y) / 2.)

    let round (p: Point2D) = xy (roundFloat p.x) (roundFloat p.y)


    /// Be careful with the vector arguments. This function is written with piping in mind. The first point is the
    /// target location. The second point is the starting location
    let vectorTo (target: Point2D) (from: Point2D) : Vector2D = target - from


    (* Json *)

    let fromList (list: float list) : Point2D option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList (point: Point2D) : float list = [ point.x; point.y ]

    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Point2D) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xy 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Point2D list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xy 0. 0.))
                :> obj
