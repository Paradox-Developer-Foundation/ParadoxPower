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
            //log "%s" f
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
            log (sprintf "Loading %s localisation in %s" localisationSettings.gameName localisationSettings.folder)

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
                log (sprintf "%s not found" localisationSettings.folder)
                YAMLLocalisationService([], localisationSettings.keyToLanguage, localisationSettings.gameToLang)
        //new (settings : CK2Settings) = HOI4LocalisationService(settings.HOI4Directory.localisationDirectory, settings.ck2Language)

        //new (settings : CK2Settings) = EU4LocalisationService(settings.EU4Directory.localisationDirectory, settings.ck2Language)
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


module EU4 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some EU4Lang.English
        | "l_french" -> Some EU4Lang.French
        | "l_spanish" -> Some EU4Lang.Spanish
        | "l_german" -> Some EU4Lang.German
        | _ -> None

    let EU4LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, EU4)

    let EU4LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Europa Universalis IV"
              keyToLanguage = keyToLanguage
              gameToLang = EU4 }

module HOI4 =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some HOI4Lang.English
        | "l_french" -> Some HOI4Lang.French
        | "l_spanish" -> Some HOI4Lang.Spanish
        | "l_german" -> Some HOI4Lang.German
        | "l_russian" -> Some HOI4Lang.Russian
        | "l_polish" -> Some HOI4Lang.Polish
        | "l_braz_por" -> Some HOI4Lang.Braz_Por
        | _ -> None

    let HOI4LocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, HOI4)

    let HOI4LocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Hearts of Iron IV"
              keyToLanguage = keyToLanguage
              gameToLang = HOI4 }

module STL =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some STLLang.English
        | "l_french" -> Some STLLang.French
        | "l_spanish" -> Some STLLang.Spanish
        | "l_german" -> Some STLLang.German
        | "l_russian" -> Some STLLang.Russian
        | "l_polish" -> Some STLLang.Polish
        | "l_braz_por" -> Some STLLang.Braz_Por
        | "l_simp_chinese" -> Some STLLang.Chinese
        | "l_japanese" -> Some STLLang.Japanese
        | "l_korean" -> Some STLLang.Korean
        | _ -> None

    let STLLocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, STL)

    let STLLocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Stellaris"
              keyToLanguage = keyToLanguage
              gameToLang = STL }

module IR =
    open YAMLLocalisationParser

    let private keyToLanguage =
        function
        | "l_english" -> Some IRLang.English
        | "l_french" -> Some IRLang.French
        | "l_german" -> Some IRLang.German
        | "l_spanish" -> Some IRLang.Spanish
        | "l_simp_chinese" -> Some IRLang.Chinese
        | "l_russian" -> Some IRLang.Russian
        | _ -> None

    let IRLocalisationService (files: (string * string) list) =
        YAMLLocalisationService(files, keyToLanguage, IR)

    let IRLocalisationServiceFromFolder (folder: string) =
        YAMLLocalisationService
            { folder = folder
              gameName = "Imperator"
              keyToLanguage = keyToLanguage
              gameToLang = IR }
