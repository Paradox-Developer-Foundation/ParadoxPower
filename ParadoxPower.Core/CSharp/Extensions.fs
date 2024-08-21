namespace ParadoxPower.CSharp

open ParadoxPower.Process
open ParadoxPower.Parser
open System.Collections.Generic
open System.Runtime.CompilerServices
open FParsec.CharParsers


[<Extension>]
type Extensions =
    /// Filter to just Nodes
    [<Extension>]
    static member inline Nodes(obj: IEnumerable<Child>) =
        obj
        |> Seq.choose (function
            | NodeChild c -> Some c
            | _ -> None)

    /// Filter to just Leaves
    [<Extension>]
    static member inline Leaves(obj: IEnumerable<Child>) =
        obj
        |> Seq.choose (function
            | LeafChild c -> Some c
            | _ -> None)

    /// Filter to just LeafValues
    [<Extension>]
    static member inline LeafValues(obj: IEnumerable<Child>) =
        obj
        |> Seq.choose (function
            | LeafValueChild c -> Some c
            | _ -> None)

    /// Filter to just Comments
    [<Extension>]
    static member inline Comments(obj: IEnumerable<Child>) =
        obj
        |> Seq.choose (function
            | CommentChild c -> Some c
            | _ -> None)

    /// Retrieve result or null (if failure)
    [<Extension>]
    static member inline GetResult(obj: ParserResult<_, _>) =
        obj
        |> function
            | Success(s, _, _) -> s
            | _ -> null

    /// Retrieve error message or null (if success)
    [<Extension>]
    static member inline GetError(obj: ParserResult<_, _>) =
        obj
        |> function
            | Failure(m, e, _) ->
                { Filename = e.Position.StreamName
                  Line = e.Position.Line
                  Column = e.Position.Column
                  ErrorMessage = m }
            | _ -> Operators.Unchecked.defaultof<ParserError>

    [<Extension>]
    static member inline PrettyPrint(obj: Statement) =
        obj |> ParadoxPower.Parser.CKPrinter.api.prettyPrintStatement
