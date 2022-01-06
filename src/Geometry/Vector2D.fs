namespace Geometry

open FSharp.Json

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2D =
    (* Builders *)

    let xy (x: float) (y: float) : Vector2D<'Length, 'Coordinates> = { x = x; y = y }

    let rTheta (r: float) (theta: Angle) : Vector2D<'Length, 'Coordinates> =
        { x = r * Angle.cos theta
          y = r * Angle.sin theta }

    let ofPolar r a = xy (r * Angle.cos a) (r * Angle.sin a)

    (* Accessors *)

    let magnitude (v: Vector2D<'Length, 'Coordinates>) = sqrt (v.x ** 2. + v.y ** 2.)


    (* Modifiers *)

    /// Scale a vector by a given amount. This scales both the x and y coordinates.
    let scaleBy (n:float) (vector: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = { x = vector.x * n; y = vector.y * n }

    /// Scale a vector to a given length.
    let scaleTo (length: float) (vector: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        scaleBy (length / magnitude vector) vector

    let mul scale (v: Vector2D<'Length, 'Coordinates>) = v * scale

    let neg (v: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> = { x = -v.x; y = -v.y }

    /// Rotate a vector counterclockwise by a given angle.
    let rotateBy a (v: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        { x = Angle.cos a * v.x - Angle.sin a * v.y
          y = Angle.sin a * v.x + Angle.cos a * v.y }
        
    /// Rotate a vector clockwise by a given angle.

    let normalize v = v / (magnitude v)

    let round (p: Vector2D<'Length, 'Coordinates>) = xy (roundFloat p.x) (roundFloat p.y)


    (* Queries *)

    let distanceSquaredTo (p1: Vector2D<'Length, 'Coordinates>) (p2: Vector2D<'Length, 'Coordinates>) : float =
        let dx = (p1.x - p2.x)
        let dy = (p1.y - p2.y)
        dx * dx + dy * dy

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    let midVector (p1: Vector2D<'Length, 'Coordinates>) (p2: Vector2D<'Length, 'Coordinates>) : Vector2D<'Length, 'Coordinates> =
        xy ((p1.x + p2.x) / 2.) ((p1.y + p2.y) / 2.)

    let dotProduct (lhs: Vector2D<'Length, 'Coordinates>) (rhs: Vector2D<'Length, 'Coordinates>) : float = (lhs.x * rhs.x) + (lhs.y * rhs.y)

    let crossProduct (lhs: Vector2D<'Length, 'Coordinates>) (rhs: Vector2D<'Length, 'Coordinates>) : float = (lhs.x * rhs.y) - (lhs.y * rhs.x)

    /// Get the direction the a vector is facing.
    let direction (vector: Vector2D<'Length, 'Coordinates>) : Direction2D option =
        if almostEqual (magnitude vector) 0. then
            None
        else
            let normalized = normalize vector
            Some(Direction2D.xy normalized.X normalized.Y)


    (* Json *)

    let fromList (list: float list) : Vector2D<'Length, 'Coordinates> option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList (vector: Vector2D<'Length, 'Coordinates>) : float list = [ vector.x; vector.y ]


    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Vector2D<'Length, 'Coordinates>) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xy 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Vector2D<'Length, 'Coordinates> list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xy 0. 0.))
                :> obj
