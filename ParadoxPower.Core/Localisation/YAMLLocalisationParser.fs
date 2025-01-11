namespace ParadoxPower.Localisation

open ParadoxPower.Utilities.Position

/// 本地化文件解析器
module YAMLLocalisationParser =
    open FParsec

    type LocFile = { key: string; entries: Entry list }

    let inline isLocValueChar (c: char) =
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

    let key = many1Satisfy ((=) ':' >> not) .>> pchar ':' .>> spaces <?> "key"

    let desc =
        many1Satisfy isLocValueChar .>>. getPosition .>>. restOfLine false <?> "desc"

    let value = digit .>> spaces <?> "version"

    let getRange (start: Position) (endp: Position) =
        mkRange start.StreamName (mkPos (int start.Line) (int start.Column)) (mkPos (int endp.Line) (int endp.Column))

    let entry =
        pipe5
            getPosition
            key
            (opt value)
            desc
            (getPosition .>> spaces)
            (fun s k v ((validDesc, endofValid), invalidDesc) e ->
                let errorRange =
                    if endofValid <> e then
                        Some(getRange endofValid e)
                    else
                        None

                { key = k
                  value = v
                  desc = validDesc + invalidDesc
                  position = getRange s e
                  errorRange = errorRange })
        <?> "entry"

    let comment = pchar '#' >>. restOfLine true .>> spaces <?> "comment"

    let file =
        spaces
        >>. many (attempt comment)
        >>. pipe2 key (many ((attempt comment |>> (fun _ -> None)) <|> (entry |>> Some)) .>> eof) (fun k es ->
            { key = k; entries = List.choose id es })
        <?> "file"

    let parseLocFile filepath =
        runParserOnFile file () filepath System.Text.Encoding.UTF8

    let parseLocText text name = runParserOnString file () name text