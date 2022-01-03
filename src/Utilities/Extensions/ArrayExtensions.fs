namespace Utilities.Extensions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Array =
    /// Return all but the last element in the array
    let inline initial (arr: _ []) = arr.[0..arr.Length - 1]
    
    
    /// Return the last element in the array
    let inline last (arr: _ []) = arr.[arr.Length - 1]
