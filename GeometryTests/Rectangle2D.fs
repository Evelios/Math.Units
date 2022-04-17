module GeometryTests.Rectangle2D

open NUnit.Framework
open FsCheck.NUnit

open Geometry

type Transformation =
    { Rectangle: Rectangle2D<Meters, TestSpace> -> Rectangle2D<Meters, TestSpace>
      Point: Point2D<Meters, TestSpace> -> Point2D<Meters, TestSpace>
      LineSegment: LineSegment2D<Meters, TestSpace> -> LineSegment2D<Meters, TestSpace> }

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

let rotation (centerPoint: Point2D<Meters, TestSpace>) (angle: Angle) : Transformation  =
    { Rectangle = Rectangle2D.rotateAround centerPoint angle
      Point = Point2D.rotateAround centerPoint angle
      LineSegment = LineSegment2D.rotateAround centerPoint angle
    }


let translation (displacement: Vector2D<Meters, TestSpace>) : Transformation =
    { Rectangle = Rectangle2D.translateBy displacement
      Point = Point2D.translateBy displacement
      LineSegment = LineSegment2D.translateBy displacement
    }


let scaling (centerPoint : Point2D<Meters, TestSpace>) (scale: Float): Transformation =
    { Rectangle = Rectangle2D.scaleAbout centerPoint scale
      Point = Point2D.scaleAbout centerPoint scale
      LineSegment = LineSegment2D.scaleAbout centerPoint scale
    }


let mirroring (axis : Axis2D<Meters, TestSpace>) : Transformation  =
    { Rectangle = Rectangle2D.mirrorAcross axis
      Point = Point2D.mirrorAcross axis
      LineSegment = LineSegment2D.mirrorAcross axis
    }
