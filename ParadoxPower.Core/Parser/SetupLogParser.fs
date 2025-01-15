namespace ParadoxPower.Parser

open FParsec
open ParadoxPower.Common

module SetupLogParser =

    type RawStaticModifier = { num: int; tag: string; name: string }
    type RawModifier = { tag: string; category: int }

    let private isvaluechar = SharedParsers.isValueChar

    let private str s =
        pstring s .>> SharedParsers.whiteSpace <?> ("string " + s)

    let private header =
        skipCharsTillString "Initializing Database: CStaticModifierDatabase" true 20000000
        .>> SharedParsers.whiteSpace
        <?> "header"

    let private pre = skipCharsTillString "Static Modifier #" true 100
    let private num = pre >>. pint64 .>> SharedParsers.whiteSpace |>> int

    let private tag =
        skipString "tag = " >>. many1Satisfy isvaluechar .>> SharedParsers.whiteSpace

    let private name = str "name = " >>. restOfLine true //manyCharsTill valuechar newline .>> ws

    let private staticModifier =
        pipe3 num tag name (fun i t n -> { num = i; tag = t; name = n })

    let private modifierHeader =
        skipCharsTillString "Printing Modifier Definitions" true 20000000
        .>> SharedParsers.whiteSpace
        <?> "modifier header"

    let private mtag =
        skipCharsTillString "Tag: " true 500
        >>. many1CharsTill (satisfy isvaluechar) (pchar ',')
        .>> SharedParsers.whiteSpace

    let private cat = skipString "Categories: " >>. pint64 |>> int
    let private modifier = pipe2 mtag cat (fun t c -> { tag = t; category = c })

    let private footer = many1Chars anyChar

    let private logFile =
        SharedParsers.whiteSpace >>. header >>. many1 (attempt staticModifier)
        .>> modifierHeader
        .>>. many1 (attempt modifier)
        .>> footer
        .>> eof


    let toDocEffect<'a when 'a: comparison> effectType parseScope (x: RawEffect) = DocEffect(x, effectType, parseScope)

    let parseLogsFile filepath =
        runParserOnFile logFile () filepath (System.Text.Encoding.GetEncoding(1252))

    let parseLogsStream file =
        runParserOnStream logFile () "logFile" file (System.Text.Encoding.GetEncoding(1252))

    let processLogs (s: RawStaticModifier list, m: RawModifier list) =
        m
        |> List.map (fun rm ->
            { ActualModifier.tag = rm.tag
              category = modifierCategoryManager.GetCategoryFromID rm.category })
