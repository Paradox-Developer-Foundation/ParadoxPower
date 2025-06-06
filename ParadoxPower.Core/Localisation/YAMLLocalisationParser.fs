namespace ParadoxPower.Localisation

open System
open System.Collections.Generic
open ParadoxPower.Parser.SharedParsers
open ParadoxPower.Utilities.Position

/// 本地化文件解析器
module YAMLLocalisationParser =
    open FParsec

    type LocFile =
        { Key: string
          Entries: Entry IReadOnlyCollection }

    let inline private isLocValueChar (c: char) =
        isAsciiLetter c
        || (c >= '\u0020' && c <= '\u007E')
        || (c >= '\u00A0' && c <= '\u024F')
        || (c >= '\u0401' && c <= '\u045F')
        || (c >= '\u0490' && c <= '\u0491')
        || (c >= '\u2013' && c <= '\u2044')
        || (c >= '\u4E00' && c <= '\u9FFF')
        || (c >= '\uFE30' && c <= '\uFE4F')
        || (c >= '\u3000' && c <= '\u30FF')
        || (c >= '\uFF00' && c <= '\uFFEF')

    let private key =
        many1Satisfy ((=) ':' >> not) .>> skipChar ':' .>> spaces <?> "key"

    let private desc =
        between (skipChar '"') (skipChar '"') (manyStrings (quotedCharSnippet <|> escapedChar) <?> "desc")
        .>>. getPosition

    let private value = digit .>> spaces <?> "version"

    let private getRange (start: Position) (endp: Position) =
        mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))

    let private entry =
        pipe5 getPosition key (opt value) desc (getPosition .>> spaces) (fun s k v (validDesc, endOfValid) e ->
            let errorRange: Nullable<Range> =
                if endOfValid <> e then
                    Nullable(getRange endOfValid e)
                else
                    Nullable()

            { Key = k
              Value = if v.IsSome then Nullable(v.Value) else Nullable()
              Desc = validDesc
              Position = getRange s e
              ErrorRange = errorRange })
        <?> "entry"

    let private comment = skipChar '#' >>. skipRestOfLine true .>> spaces <?> "comment"

    let private manyOption p =
        Inline.Many(
            firstElementParser = p,
            elementParser = p,
            stateFromFirstElement =
                (fun (value: Option<'a>) ->
                    let list = new LinkedList<'a>()

                    if value.IsSome then
                        list.AddLast(value.Value) |> ignore

                    list),
            foldState =
                (fun (list: LinkedList<'a>) newValue ->
                    if newValue.IsSome then
                        list.AddLast(newValue.Value) |> ignore

                    list),
            resultFromState = id,
            resultForEmptySequence = (fun () -> LinkedList<_>())
        )

    let private file =
        spaces
        >>. many (attempt comment)
        >>. pipe2 key (manyOption ((attempt comment >>% None) <|> (entry |>> Some)) .>> eof) (fun k es ->
            { Key = k; Entries = es })
        <?> "file"

    let ParseLocFile filepath =
        runParserOnFile file () filepath System.Text.Encoding.UTF8

    let ParseLocText text name = runParserOnString file () name text
