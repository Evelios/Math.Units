namespace Utilities.Extensions

[<RequireQualifiedAccess>]
module View =
    open Avalonia.Controls
    open Avalonia.FuncUI.Types
    open Avalonia.LogicalTree
    open System
    

    /// Add an attribute to an existing view
    let withAttr (attr: IAttr<'view>) (view: IView<'view>) =
        { viewType = view.ViewType
          attrs = attr :: view.Attrs }
        :> IView<'view>

    /// Add several attributes to an existing view
    let withAttrs (attrs: IAttr<'view> list) (view: IView<'view>) =
        { viewType = view.ViewType
          attrs = view.Attrs |> List.append attrs }
        :> IView<'view>

    /// Try to find a child control of a given name using breadth first search
    let findChildControl (name: string) (source: IControl) : IControl option =
        let rec findChildControlHelper (children: ILogical list) =
            match children with
            | first :: remaining ->
                if (first :?> IControl).Name = name then
                    Some(first :?> IControl)
                else
                    findChildControlHelper (remaining @ (List.ofSeq first.LogicalChildren))

            | [] -> None

        findChildControlHelper (List.ofSeq source.LogicalChildren)

    /// Traverse to the root of the tree and do a breadth first search for the element
    let findControl (name: String) (source: IControl) : IControl option =
        if source.Name = name then
            Some source
        else
            findChildControl name (source.VisualRoot :?> IControl)
