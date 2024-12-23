namespace ParadoxPower.Localisation

open ParadoxPower.Common
open ParadoxPower.Utilities.Position
open System.Collections.Generic
open System.IO
open ParadoxPower.Utilities.Utils

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

    let getRange (start: FParsec.Position) (endp: FParsec.Position) =
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

    let comment = pstring "#" >>. restOfLine true .>> spaces <?> "comment"

    let file =
        spaces
        >>. many (attempt comment)
        >>. pipe2 key (many ((attempt comment |>> (fun _ -> None)) <|> (entry |>> Some)) .>> eof) (fun k es ->
            { key = k; entries = List.choose id es })
        <?> "file"

    let parseLocFile filepath =
        runParserOnFile file () filepath System.Text.Encoding.UTF8

    let parseLocText text name = runParserOnString file () name text

    type YAMLLocalisationService<'L>(files: (string * string) list, keyToLanguage, gameLang) =
        let mutable results: Results =
            upcast new Dictionary<string, bool * int * string * Position option>()

        let mutable records: struct (Entry * Lang) array = [||]
        let mutable recordsL: struct (Entry * Lang) list = []

        let addFile f t =
            match parseLocText t f with
            | Success({ key = key; entries = entries }, _, _) ->
                match keyToLanguage key with
                | Some l ->
                    let es = entries |> List.map (fun e -> struct (e, gameLang l))
                    recordsL <- es @ recordsL
                    (true, es.Length, "", None)
                | None -> (true, entries.Length, "", None)
            | Failure(msg, p, _) -> (false, 0, msg, Some p.Position)

        let addFiles (x: (string * string) list) =
            List.map (fun (f, t) -> (f, addFile f t)) x

        let recordsLang (lang: Lang) =
            records
            |> Array.choose (function
                | struct (r, l) when l = lang -> Some r
                | _ -> None)
            |> List.ofArray

        let valueMap lang =
            recordsLang lang |> List.map (fun r -> (r.key, r)) |> Map.ofList

        let values l =
            recordsLang l |> List.map (fun r -> (r.key, r.desc)) |> dict

        let getDesc l x =
            recordsLang l
            |> List.tryPick (fun r -> if r.key = x then Some r.desc else None)
            |> Option.defaultValue x

        let getKeys l =
            recordsLang l |> List.map (fun r -> r.key)

        do
            results <- addFiles files |> dict
            records <- recordsL |> Array.ofList
            recordsL <- []

        new(localisationSettings: LocalisationSettings<'L>) =
            log $"Loading %s{localisationSettings.gameName} localisation in %s{localisationSettings.folder}"

            match Directory.Exists(localisationSettings.folder) with
            | true ->
                let files =
                    Directory.EnumerateDirectories localisationSettings.folder
                    |> List.ofSeq
                    |> List.collect (Directory.EnumerateFiles >> List.ofSeq)

                let rootFiles = Directory.EnumerateFiles localisationSettings.folder |> List.ofSeq

                let actualFiles =
                    files @ rootFiles
                    |> List.map (fun f -> f, File.ReadAllText(f, System.Text.Encoding.UTF8))

                YAMLLocalisationService(
                    actualFiles,
                    localisationSettings.keyToLanguage,
                    localisationSettings.gameToLang
                )
            | false ->
                log $"%s{localisationSettings.folder} not found"
                YAMLLocalisationService([], localisationSettings.keyToLanguage, localisationSettings.gameToLang)

        member _.Api lang =
            { new ILocalisationAPI with
                member _.Results = results
                member _.Values = values lang
                member _.GetKeys = getKeys lang
                member _.GetDesc x = getDesc lang x
                member _.GetLang = lang
                member _.ValueMap = valueMap lang }

        interface ILocalisationAPICreator with
            member this.Api l = this.Api l