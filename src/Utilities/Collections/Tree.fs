namespace Utilities.Collections


type Tree<'INodeData> =
    | LeafNode of 'INodeData
    | InternalNode of 'INodeData * Tree<'INodeData> seq

/// The data needed to create a tree object. Start creating the tree from the base case
type TreeInitializer<'INodeData> =
    { BaseCase: 'INodeData
      Continuation: 'INodeData -> 'INodeData seq
      Termination: 'INodeData -> bool }

module Tree =
    (* Builders *)

    let fromLeaf = LeafNode

    let fromNode node tree = InternalNode(node, tree)

    /// Create a tree from the tree initializer data structure. This starts with the base case and grows the tree with
    /// the continuation function until the termination condition is reached.
    let fromInitializer initializer =
        let rec createNode step =
            if initializer.Termination step then
                fromLeaf step

            else
                fromNode step (Seq.map createNode (initializer.Continuation step))


        createNode initializer.BaseCase


    (* Modifiers *)

    let rec cata fLeaf fNode (tree: Tree<'INodeData>) : 'r =
        let recurse = cata fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) -> fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold folder acc (tree: Tree<'INodeData>) : 'r =
        let recurse = fold folder

        match tree with
        | LeafNode leafInfo -> folder acc leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            // determine the local accumulator at this level
            let localAccum = folder acc nodeInfo
            // thread the local accumulator through all the sub-items using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum
            // ... and return it
            finalAccum

    (* Accessors *)
    let nodes tree = fold (fun sum _ -> sum + 1) 0 tree
