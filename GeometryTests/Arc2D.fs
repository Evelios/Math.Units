module GeometryTests.Arc2D

open NUnit.Framework
open FsCheck.NUnit
open FsCheck

open Geometry

[<SetUp>]
let Setup () = Gen.ArbGeometry.Register()

[<Test>]
let ``Empty Test`` () = Assert.Pass()


[<Property>]
let ``Evaluating at t=0 returns start point`` (arc: Arc2D<Meters, TestSpace>) =
    Test.equal (Arc2D.startPoint arc) (Arc2D.pointOn arc 0.)


[<Property>]
let ``Evaluating at t=1 returns end point`` (arc: Arc2D<Meters, TestSpace>) =
    Test.equal (Arc2D.endPoint arc) (Arc2D.pointOn arc 1.)

[<Property>]
let ``Evaluating at t=0.5 returns midpoint`` (arc: Arc2D<Meters, TestSpace>) =
    Test.equal (Arc2D.midpoint arc) (Arc2D.pointOn arc 0.5)

[<Property>]
let ``Reversing an arc keeps it's midpoint`` (arc: Arc2D<Meters, TestSpace>) =
    Test.equal (Arc2D.midpoint arc) (Arc2D.reverse arc |> Arc2D.midpoint)

[<Property>]
let ``Reversing an arc is consistent with reversed evaluation`` (arc: Arc2D<Meters, TestSpace>) =
    let genT = Gen.floatBetween 0. 1. |> Arb.fromGen

    Prop.forAll genT (fun t -> Test.equal (Arc2D.pointOn (Arc2D.reverse arc) t) (Arc2D.pointOn arc (1. - t)))


[<Property>]
let ``Arc2D.from produces the expected endpoint``
    (start: Point2D<Meters, TestSpace>)
    (finish: Point2D<Meters, TestSpace>)
    =

    let validAngle =
        Gen.map
            Angle.degrees
            (Gen.oneof [
                Gen.floatBetween -359. 359.
                Gen.floatBetween 361. 719.
                Gen.floatBetween -719. -361.
             ])
        |> Arb.fromGen

    Prop.forAll
        validAngle
        (fun sweptAngle ->
            let arc = Arc2D.from start finish sweptAngle

            Test.all [
                Test.equal start (Arc2D.startPoint arc)
                Test.equal finish (Arc2D.endPoint arc)
            ])

[<Property>]
[<Ignore("I need to figure this out")>]
let ``Arc2D.withRadius produces the expected endpoint``
    (Positive radius: Length<Meters> Positive)
    (sweptAngle: SweptAngle)
    (startPoint: Point2D<Meters, TestSpace>)
    (endPoint: Point2D<Meters, TestSpace>)
    =

    match Arc2D.withRadius radius sweptAngle startPoint endPoint with

    | Some arc -> Arc2D.endPoint arc .=. endPoint 

    | None ->
        let distance = Point2D.distanceTo startPoint endPoint

        if distance = Length.zero then
            Test.pass
        else
            distance .>=. 2. * radius
