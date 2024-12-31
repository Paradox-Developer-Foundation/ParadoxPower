namespace ParadoxPower.CSharp

open FParsec.Internals
open ParadoxPower.Languages
open ParadoxPower.Process
open ParadoxPower.Parser
open System.Collections.Generic
open System.Runtime.CompilerServices
open FParsec.CharParsers

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
            | Failure(_, e, _) ->
                { Filename = e.Position.StreamName
                  Line = e.Position.Line
                  Column = e.Position.Column
                  ErrorMessage = Extensions.GetErrorMessage(e) }
            | _ -> Operators.Unchecked.defaultof<ParserError>

    // 获取本地化的错误信息
    // 参考: https://github.com/stephan-tolksdorf/fparsec/blob/master/FParsec/Error.fs#L279
    static member GetErrorMessage(parseError: FParsec.Error.ParserError) =
        let indentationIncrement = "  "
        use stringWriter = new System.IO.StringWriter()
        let lineWrapper = LineWrapper(stringWriter, 79)

        let rec printMessages (messageList: FParsec.ErrorMessageList) =
            let nra () = ResizeArray<_>()

            let expectedA, unexpectedA, messageA, nestedA, compoundA =
                nra (), nra (), nra (), nra (), nra ()

            let mutable otherCount = 0

            for message in FParsec.ErrorMessageList.ToSortedArray(messageList) do
                match message with
                | FParsec.Error.Expected e -> expectedA.Add(e)
                | FParsec.Error.ExpectedString es -> expectedA.Add($"'{es}'")
                | FParsec.Error.ExpectedStringCI es -> expectedA.Add($"'{es}' ({Resources.Parse_CaseInsensitive})")
                | FParsec.Error.Unexpected u -> unexpectedA.Add(u)
                | FParsec.Error.UnexpectedString us -> unexpectedA.Add($"'{us}'")
                | FParsec.Error.UnexpectedStringCI us -> messageA.Add($"'{us}' ({Resources.Parse_CaseInsensitive})")
                | FParsec.Error.Message m -> messageA.Add(m)
                | FParsec.Error.NestedError _ ->
                    let ne = message :?> FParsec.ErrorMessage.NestedError
                    nestedA.Add((ne.Position, ne.Messages))
                | FParsec.Error.CompoundError _ ->
                    let ce = message :?> FParsec.ErrorMessage.CompoundError

                    if not (System.String.IsNullOrEmpty ce.LabelOfCompound) then
                        expectedA.Add(ce.LabelOfCompound)

                    compoundA.Add((ce.LabelOfCompound, ce.NestedErrorPosition, ce.NestedErrorMessages))
                | FParsec.Error.OtherErrorMessage _ -> otherCount <- otherCount + 1
                | _ -> failwith "printMessages"

            let printArray title (a: ResizeArray<string>) (sep: string) =
                lineWrapper.Print(title, " ")
                let n = a.Count

                for i = 0 to n - 3 do
                    lineWrapper.Print(a[i], ", ")

                if n > 1 then
                    lineWrapper.Print(a[n - 2], sep)

                if n > 0 then
                    lineWrapper.Print(a[n - 1])

                lineWrapper.Newline()

            if expectedA.Count > 0 then
                printArray Resources.Parse_Expected expectedA Resources.Parse_Or

            if unexpectedA.Count > 0 then
                printArray Resources.Parse_Unexpected unexpectedA Resources.Parse_And

            let ind = lineWrapper.Indentation
            let indInd = ind + indentationIncrement

            if messageA.Count > 0 then
                if expectedA.Count > 0 || unexpectedA.Count > 0 then
                    lineWrapper.PrintLine(Resources.Parse_OtherErrors)
                    lineWrapper.Indentation <- indInd

                for m in messageA do
                    lineWrapper.PrintLine(m)

                if expectedA.Count > 0 || unexpectedA.Count > 0 then
                    lineWrapper.Indentation <- ind

                for label, _, msgs2 in compoundA do
                    lineWrapper.Newline()
                    lineWrapper.PrintLine(System.String.Format(Resources.Parse_CompoundCouldNotBeParsedBecause, label))
                    lineWrapper.Indentation <- indInd
                    printMessages msgs2
                    lineWrapper.Indentation <- ind

                for _, msgs2 in nestedA do
                    lineWrapper.Newline()
                    lineWrapper.PrintLine(Resources.Parse_ParserBacktrackedAfter)
                    lineWrapper.Indentation <- indInd
                    printMessages msgs2
                    lineWrapper.Indentation <- ind

                if
                    expectedA.Count = 0
                    && unexpectedA.Count = 0
                    && messageA.Count = 0
                    && compoundA.Count = 0
                    && nestedA.Count = 0
                then
                    lineWrapper.PrintLine(Resources.Parse_UnknownErrors)

        printMessages parseError.Messages
        stringWriter.ToString()

    [<Extension>]
    static member inline PrettyPrint(obj: Statement) = obj |> CKPrinter.PrettyPrintStatement
