module GeometryTests.Rectangle2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry

type Transformation =
    { Rectangle: Rectangle2D<Meters, TestSpace> -> Rectangle2D<Meters, TestSpace>
      Point: Point2D<Meters, TestSpace> -> Point2D<Meters, TestSpace>
      LineSegment: LineSegment2D<Meters, TestSpace> -> LineSegment2D<Meters, TestSpace> }




let rotation (centerPoint: Point2D<Meters, TestSpace>) (angle: Angle) : Transformation =
    { Rectangle = Rectangle2D.rotateAround centerPoint angle
      Point = Point2D.rotateAround centerPoint angle
      LineSegment = LineSegment2D.rotateAround centerPoint angle }


let translation (displacement: Vector2D<Meters, TestSpace>) : Transformation =
    { Rectangle = Rectangle2D.translateBy displacement
      Point = Point2D.translateBy displacement
      LineSegment = LineSegment2D.translateBy displacement }


let scaling (centerPoint: Point2D<Meters, TestSpace>) (scale: float) : Transformation =
    { Rectangle = Rectangle2D.scaleAbout centerPoint scale
      Point = Point2D.scaleAbout centerPoint scale
      LineSegment = LineSegment2D.scaleAbout centerPoint scale }


let mirroring (axis: Axis2D<Meters, TestSpace>) : Transformation =
    { Rectangle = Rectangle2D.mirrorAcross axis
      Point = Point2D.mirrorAcross axis
      LineSegment = LineSegment2D.mirrorAcross axis }


let transformation =
    Gen.oneof [
        Gen.map2 rotation Gen.point2D Gen.angle
        Gen.map translation Gen.vector2D
        Gen.map2 scaling Gen.point2D Gen.float
        Gen.map mirroring Gen.axis2D
    ]

type ArbTransformation =
    static member Transformation() = Arb.fromGen transformation

    static member Register() =
        Arb.register<ArbTransformation> () |> ignore

[<SetUp>]
let Setup () =
    Gen.ArbGeometry.Register()
    ArbTransformation.Register()


[<Property>]
[<Ignore("I need to figure this out")>]
let ``Rectangle/point containment is consistent through transformation``
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    (point: Point2D<Meters, TestSpace>)
    =

    let initialContainment = Rectangle2D.contains point rectangle
    let transformedPoint = transformation.Point point
    let transformedRectangle = transformation.Rectangle rectangle

    let finalContainment =
        Rectangle2D.contains transformedPoint transformedRectangle

    Test.equal initialContainment finalContainment


let verticesTransformation
    (accessor: Rectangle2D<Meters, TestSpace> -> Point2D<Meters, TestSpace>)
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    =
    let vertex = accessor rectangle
    let transformedRectangle = transformation.Rectangle rectangle
    let transformedVertex = transformation.Point vertex
    let vertexOfTransformed = accessor transformedRectangle

    Test.equal transformedVertex vertexOfTransformed

[<Property>]
[<Ignore("I need to figure this out")>]
let ``Vertices are consistent through bottom left transformation``
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    =
    verticesTransformation (fun r -> Rectangle2D.interpolate r 0. 0.) transformation rectangle

[<Property>]
[<Ignore("I need to figure this out")>]
let ``Vertices are consistent through bottom right transformation``
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    =
    verticesTransformation (fun r -> Rectangle2D.interpolate r 1. 0.) transformation rectangle

[<Property>]
[<Ignore("I need to figure this out")>]
let ``Vertices are consistent through top left transformation``
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    =
    verticesTransformation (fun r -> Rectangle2D.interpolate r 1. 1.) transformation rectangle

[<Property>]
[<Ignore("I need to figure this out")>]
let ``Vertices are consistent through top right transformation``
    (transformation: Transformation)
    (rectangle: Rectangle2D<Meters, TestSpace>)
    =
    verticesTransformation (fun r -> Rectangle2D.interpolate r 0. 1.) transformation rectangle
