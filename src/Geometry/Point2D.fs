namespace Geometry

open FSharp.Json

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Point2D =
    (* Builders *)

    let xy (x: float) (y: float) : Point2D<'Length, 'Coordinates> = { x = x; y = y }

    let rTheta (r: float) (theta: Angle) : Point2D<'Length, 'Coordinates> =
        { x = r * Angle.cos theta
          y = r * Angle.sin theta }

    let ofPolar r a = xy (r * Angle.cos a) (r * Angle.sin a)

    let origin () : Point2D<'Length, 'Coordinates> = xy 0. 0.

    (* Modifiers *)

    let toVector (point: Point2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = Vector2D.xy point.x point.y

    let scale x y (point: Point2D<'Length, 'Coordinates>) : Point2D<'Length, 'Coordinates> =
        { x = point.x * x; y = point.y * y }

    let translate (v: Vector2D<'Length, 'Coordinates>) (p: Point2D<'Length, 'Coordinates>) = p + v

    let rotateAround
        (reference: Point2D<'Length, 'Coordinates>)
        (angle: Angle)
        (point: Point2D<'Length, 'Coordinates>)
        : Point2D<'Length, 'Coordinates> =
        let c = Angle.cos angle
        let s = Angle.sin angle
        let deltaX = point.x - reference.x
        let deltaY = point.y - reference.y

        { x = reference.x + c * deltaX - s * deltaY
          y = reference.y + s * deltaX + c * deltaY }

    let placeIn
        (frame: Frame2D<'Length, 'Coordinates>)
        (point: Point2D<'Length, 'Coordinates>)
        : Point2D<'Length, 'Coordinates> =
        let i = frame.XDirection
        let j = frame.YDirection

        { x = frame.Origin.x + point.X * i.X + point.Y * j.X
          y = frame.Origin.y + point.X * i.Y + point.Y * j.Y }

    (* Queries *)

    let distanceSquaredTo (p1: Point2D<'Length, 'Coordinates>) (p2: Point2D<'Length, 'Coordinates>) : float =
        let dx = (p1.x - p2.x)
        let dy = (p1.y - p2.y)
        dx * dx + dy * dy

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    let midpoint
        (p1: Point2D<'Length, 'Coordinates>)
        (p2: Point2D<'Length, 'Coordinates>)
        : Point2D<'Length, 'Coordinates> =
        xy ((p1.x + p2.x) / 2.) ((p1.y + p2.y) / 2.)

    let round (p: Point2D<'Length, 'Coordinates>) = xy (roundFloat p.x) (roundFloat p.y)


    /// Be careful with the vector arguments. This function is written with piping in mind. The first point is the
    /// target location. The second point is the starting location
    let vectorTo
        (target: Point2D<'Length, 'Coordinates>)
        (from: Point2D<'Length, 'Coordinates>)
        : Vector2D<'Length, 'Coordinates> =
        target - from


    (* Json *)

    let fromList (list: float list) : Point2D<'Length, 'Coordinates> option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList (point: Point2D<'Length, 'Coordinates>) : float list = [ point.x; point.y ]

    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()

            member this.toTargetType value =
                toList (value :?> Point2D<'Length, 'Coordinates>) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xy 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Point2D<'Length, 'Coordinates> list
                |> List.map toList
                :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xy 0. 0.))
                :> obj
