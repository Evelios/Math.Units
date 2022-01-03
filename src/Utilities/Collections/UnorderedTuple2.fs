namespace Utilities.Collections

open System

[<CustomEquality>]
[<CustomComparison>]
type UnorderedTuple2<'a when 'a: comparison and 'a: equality> =
    | UnorderedTuple2 of 'a * 'a


    interface IComparable<UnorderedTuple2<'a>> with
        member this.CompareTo(tuple) = this.Comparison(tuple)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? (UnorderedTuple2<'a>) as tuple -> this.Comparison(tuple)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: UnorderedTuple2<'a>) =
        match this, other with
        | UnorderedTuple2 (lhs1, lhs2), UnorderedTuple2 (rhs1, rhs2) ->
            let lhsMin = min lhs1 lhs2
            let rhsMin = min rhs1 rhs2
            let lhsMax = max lhs1 lhs2
            let rhsMax = max rhs1 rhs2

            if lhsMin = rhsMin then
                lhsMax < rhsMax
            else
                lhsMin < rhsMin

    override this.Equals(obj: obj) =
        match obj with
        | :? (UnorderedTuple2<'a>) as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: UnorderedTuple2<'a>) =
        match this, other with
        | UnorderedTuple2 (lhs1, lhs2), UnorderedTuple2 (rhs1, rhs2) ->
            (lhs1 = rhs1 && lhs2 = rhs2)
            || (lhs1 = rhs2 && lhs2 = rhs1)

    override this.GetHashCode() =
        match this with
        // the xor operator (^^^) provides order agnostic hash combination
        | UnorderedTuple2 (left, right) -> left.GetHashCode() ^^^ right.GetHashCode()


module UnorderedTuple2 =
    let ofTuple = UnorderedTuple2
    let from a b = ofTuple (a, b)
    let fst (UnorderedTuple2 (a, _)) = a
    let snd (UnorderedTuple2 (_, b)) = b
    let toTuple (UnorderedTuple2 (a, b)) = (a, b)
    let map f (UnorderedTuple2 (a, b)) = (f a, f b)
