namespace ParadoxPower.Common

open System.Collections.Generic
open System
open ParadoxPower.Utilities

type EntityType =
        | Agenda = 1
        | AmbientObjects = 2
        | Anomalies = 3
        | Armies = 4
        | ArmyAttachments = 5
        | AscensionPerks = 6
        | Attitudes = 7
        | BombardmentStances = 8
        | BuildablePops = 9
        | BuildingTags = 10
        | Buildings = 11
        | ButtonEffects = 12
        | Bypass = 13
        | CasusBelli = 14
        | Colors = 15
        | ComponentFlags = 16
        | ComponentSets = 17
        | ComponentTags = 18
        | ComponentTemplates = 19
        | CountryCustomization = 20
        | CountryTypes = 21
        | Deposits = 22
        | DiploPhrases = 23
        | DiplomaticActions = 24
        | Edicts = 25
        | Ethics = 26
        | EventChains = 27
        | FallenEmpires = 28
        | GameRules = 29
        | GlobalShipDesigns = 30
        | Governments = 31
        | Authorities = 90
        | Civics = 32
        | GraphicalCulture = 33
        | Mandates = 34
        | MapModes = 35
        | Megastructures = 36
        | NameLists = 37
        | NotificationModifiers = 38
        | ObservationStationMissions = 39
        | OnActions = 40
        | OpinionModifiers = 41
        | Personalities = 42
        | PlanetClasses = 43
        | PlanetModifiers = 44
        | Policies = 45
        | PopFactionTypes = 46
        | PrecursorCivilizations = 47
        | ScriptedEffects = 48
        | ScriptedLoc = 49
        | ScriptedTriggers = 50
        | ScriptedVariables = 51
        | SectionTemplates = 52
        | SectorTypes = 53
        | ShipBehaviors = 54
        | ShipSizes = 55
        | SolarSystemInitializers = 56
        | SpecialProjects = 57
        | SpeciesArchetypes = 58
        | SpeciesClasses = 59
        | SpeciesNames = 60
        | SpeciesRights = 61
        | StarClasses = 62
        | StarbaseBuilding = 63
        | StarbaseLevels = 64
        | StarbaseModules = 65
        | StarbaseTypes = 66
        | SpaceportModules = 67
        | StartScreenMessages = 68
        | StaticModifiers = 69
        | StrategicResources = 70
        | Subjects = 71
        | SystemTypes = 72
        | Technology = 73
        | Terraform = 74
        | TileBlockers = 75
        | TraditionCategories = 76
        | Traditions = 77
        | Traits = 78
        | TriggeredModifiers = 79
        | WarDemandCounters = 80
        | WarDemandTypes = 81
        | WarGoals = 82
        | Events = 83
        | MapGalaxy = 84
        | MapSetupScenarios = 85
        | PrescriptedCountries = 86
        | Interface = 87
        | GfxGfx = 88
        | Other = 89
        | GfxAsset = 90
        | Decisions = 91

type RawEffect =
    { name: string
      desc: string
      usage: string
      scopes: string list
      targets: string list
      traits: string option }

[<AutoOpen>]
module rec NewScope =
    open ParadoxPower.Utilities.Utils

    type ScopeInput =
        { name: string
          aliases: string list
          isSubscopeOf: string list
          dataTypeName: string option }

    type ScopeWrapper = byte

    type ScopeManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, Scope>()
        let mutable reverseDict = Dictionary<Scope, ScopeInput>()
        let mutable groupDict = Map<string, Scope list>(Seq.empty)
        let mutable complexEquality = false
        let mutable matchesSet = Set<Scope * Scope>(Seq.empty)
        let mutable dataTypeMap = Map<Scope, string>(Seq.empty)
        let anyScope = Scope(0uy)

        let anyScopeInput =
            { ScopeInput.name = "Any"
              aliases = [ "any"; "all"; "no_scope"; "none" ]
              isSubscopeOf = []
              dataTypeName = None }

        let invalidScope = Scope(1uy)

        let invalidScopeInput =
            { ScopeInput.name = "Invalid"
              aliases = [ "invalid_scope" ]
              isSubscopeOf = []
              dataTypeName = None }

        let parseScope () =
            if not initialized then
                ParadoxPower.Utilities.Utils.logError "Error: parseScope was used without initializing scopes"
            else
                ()

            (fun (x: string) ->
                let found, value = dict.TryGetValue(x.ToLower())

                if found then
                    value
                else
                    log (sprintf "Unexpected scope %O" x)
                    anyScope)

        let init (scopes: ScopeInput list, scopeGroups: (string * string list) list) =
            initialized <- true
            // log (sprintfn "Init scopes %A" scopes)
            dict <- Dictionary<string, Scope>()
            reverseDict <- Dictionary<Scope, ScopeInput>()
            dict.Add("any", anyScope)
            dict.Add("all", anyScope)
            dict.Add("no_scope", anyScope)
            dict.Add("none", anyScope)
            dict.Add("invalid_scope", invalidScope)
            reverseDict.Add(anyScope, anyScopeInput)
            reverseDict.Add(invalidScope, invalidScopeInput)
            let mutable nextByte = 2uy

            let addScope (newScope: ScopeInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                newScope.aliases |> List.iter (fun s -> dict.Add(s, Scope(newID)))
                reverseDict.Add(Scope(newID), newScope)

                match newScope.dataTypeName with
                | Some dtn -> dataTypeMap <- dataTypeMap |> Map.add (Scope(newID)) dtn
                | None -> ()

            scopes |> List.iter addScope

            let addScopeSubset (newScope: ScopeInput) =
                newScope.isSubscopeOf
                |> List.iter (fun ss ->
                    matchesSet <-
                        (Set.add (parseScope () (newScope.aliases |> List.head), parseScope () ss) matchesSet))

            scopes |> List.iter addScopeSubset

            if Set.isEmpty matchesSet then
                ()
            else
                complexEquality <- true

            groupDict <-
                scopeGroups
                |> List.map (fun (name, scopes) -> name, (scopes |> List.map (fun s -> parseScope () s)))
                |> Map.ofList

        member this.GetName(scope: Scope) =
            let found, value = reverseDict.TryGetValue scope

            if found then
                value.name
            else
                log (sprintf "Unexpected scope %O" scope.tag)
                ""

        member this.AllScopes = reverseDict.Keys |> List.ofSeq
        member this.AnyScope = anyScope
        member this.InvalidScope = invalidScope
        member this.ParseScope = parseScope
        member this.ScopeGroups = groupDict

        member this.ParseScopes =
            function
            | "all" -> this.AllScopes
            | x -> [ this.ParseScope () x ]

        member this.ReInit(scopes: ScopeInput list, scopeGroups: (string * string list) list) =
            init (scopes, scopeGroups)

        member this.MatchesScope (source: Scope) (target: Scope) =
            if not complexEquality then
                match source, target with
                | x, _
                | _, x when x = anyScope -> true
                | x, y -> x = y
            else
                match Set.contains (source, target) matchesSet, source, target with
                | true, _, _ -> true
                | _, x, _
                | _, _, x when x = anyScope -> true
                | _, x, y -> x = y

        member this.DataTypeForScope(scope: Scope) =
            Map.tryFind scope dataTypeMap |> Option.defaultValue (scope.ToString())

        member this.Initialized = initialized

    let scopeManager = ScopeManager()

    type ModifierCategoryInput =
        { name: string
          internalID: int option
          scopes: Scope list }

    type ModifierCategoryManager() =
        let mutable initialized = false
        let mutable dict = Dictionary<string, ModifierCategory>()

        let mutable reverseDict = Dictionary<ModifierCategory, ModifierCategoryInput>()

        let mutable matchesSet = Set<ModifierCategory * Scope>(Seq.empty)
        let mutable idMap = Map<int, ModifierCategory>([])
        let anyModifier = ModifierCategory(0uy)

        let anyModifierInput =
            { ModifierCategoryInput.name = "Any"
              internalID = None
              scopes = [ scopeManager.AnyScope ] }

        let invalidModifier = ModifierCategory(1uy)

        let invalidModifierInput =
            { ModifierCategoryInput.name = "Invalid"
              internalID = None
              scopes = [] }

        let parseModifierCategory () =
            if not initialized then
                ParadoxPower.Utilities.Utils.logError
                    "Error: parseModifierCategory was used without initializing modifier categories"
            else
                ()

            (fun (x: string) ->
                let found, value = dict.TryGetValue(x.ToLower())

                if found then
                    value
                else
                    log (sprintf "Unexpected modifier category %O" x)
                    anyModifier)

        let init (modifiers: ModifierCategoryInput list) =
            initialized <- true
            // log (sprintfn "Init scopes %A" scopes)
            dict <- Dictionary<string, ModifierCategory>()
            reverseDict <- Dictionary<ModifierCategory, ModifierCategoryInput>()
            dict.Add("any", anyModifier)
            dict.Add("invalid_modifier", invalidModifier)
            reverseDict.Add(anyModifier, anyModifierInput)
            reverseDict.Add(invalidModifier, invalidModifierInput)
            let mutable nextByte = 2uy

            let addModifier (newModifier: ModifierCategoryInput) =
                let newID = nextByte
                nextByte <- nextByte + 1uy
                let modifier = ModifierCategory(newID)
                dict.Add(newModifier.name.ToLower(), modifier)
                reverseDict.Add(modifier, newModifier)

                newModifier.scopes
                |> List.iter (fun s -> matchesSet <- matchesSet |> Set.add (modifier, s))

                match newModifier.internalID with
                | Some id -> idMap <- idMap |> (Map.add id modifier)
                | None -> ()

            modifiers |> List.iter addModifier

        member this.GetName(modifier: ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier

            if found then
                value.name
            else
                log (sprintf "Unexpected modifier category %O" modifier.tag)
                ""

        member this.AllModifiers = reverseDict.Keys |> List.ofSeq
        member this.AnyModifier = anyModifier
        member this.InvalidModifiere = invalidModifier
        member this.ParseModifier = parseModifierCategory
        member this.ReInit(modifiers: ModifierCategoryInput list) = init modifiers

        member this.SupportsScope (source: ModifierCategory) (target: Scope) =
            match Set.contains (source, target) matchesSet, source, target with
            | true, _, _ -> true
            | _, x, _ when x = anyModifier -> true
            | _, _, x when x = scopeManager.AnyScope -> true
            | _ -> false

        member this.SupportedScopes(modifier: ModifierCategory) =
            let found, value = reverseDict.TryGetValue modifier

            if found then
                value.scopes
            else
                log (sprintf "Unexpected modifier category %O" modifier.tag)
                []

        member this.GetCategoryFromID(id: int) =
            match idMap |> Map.tryFind id with
            | Some category -> category
            | None -> anyModifier

        member this.Initialized = initialized

    let modifierCategoryManager = ModifierCategoryManager()

    type ModifierCategory(tag: byte) =
        member val tag = tag
        override x.ToString() = modifierCategoryManager.GetName(x)

        override x.Equals(target: obj) =
            match target with
            | :? ModifierCategory as t -> tag = t.tag
            | _ -> false

        override x.GetHashCode() = tag.GetHashCode()

        interface IComparable with
            member this.CompareTo target =
                match target with
                | :? ModifierCategory as t -> tag.CompareTo t.tag
                | _ -> 0

        member this.SupportsScope x =
            modifierCategoryManager.SupportsScope this x

        member this.Name = modifierCategoryManager.GetName this

    type Modifier = ModifierCategory

    type Scope(tag: byte) =
        member val tag = tag
        override x.ToString() = scopeManager.GetName(x)

        override x.Equals(target: obj) =
            match target with
            | :? Scope as t -> tag = t.tag
            | _ -> false

        override x.GetHashCode() = tag.GetHashCode()

        interface IComparable with
            member this.CompareTo target =
                match target with
                | :? Scope as t -> tag.CompareTo t.tag
                | _ -> 0

        member this.AnyScope = scopeManager.AnyScope

        member this.IsOfScope target =
            match this, target with
            | _, x
            | x, _ when x = scopeManager.AnyScope -> true
            | this, target -> scopeManager.MatchesScope this target


    type TypeDefInfo =
        { id: string
          validate: bool
          range: ParadoxPower.Utilities.Position.Range
          explicitLocalisation: (string * string * bool) list
          subtypes: string list }

type ActualModifier =
    { tag: string
      // source: ModifierSource
      category: ModifierCategory }

type EffectType =
    | Effect
    | Trigger
    | Link
    | ValueTrigger

type ReferenceHint =
    | TypeRef of typeName: string * typeValue: string
    | LocRef of locKey: string
    | EnumRef of enumName: string * enumValue: string
    | FileRef of filename: string

type Effect internal (name, scopes, effectType, refHint) =
    member val Name: StringTokens = name
    member val Scopes: Scope list = scopes
    member val Type: EffectType = effectType
    member val RefHint: ReferenceHint option = refHint


    override x.Equals(y) =
        match y with
        | :? Effect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        | _ -> false

    override x.GetHashCode() =
        hash (x.Name, x.Scopes, x.Type)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y ->
                let r1 = x.Name.normal.CompareTo(y.Name.normal)

                if r1 = 0 then
                    0
                else
                    List.compareWith compare x.Scopes y.Scopes
            | _ -> invalidArg "yobj" ("cannot compare values of different types" + yobj.GetType().ToString())

    override x.ToString() =
        sprintf "%s: %A" (StringResource.stringManager.GetStringForIDs x.Name) x.Scopes

    new(name: StringTokens, scopes, effectType) = Effect(name, scopes, effectType, None)

    new(name: string, scopes, effectType) =
        Effect(StringResource.stringManager.InternIdentifierToken name, scopes, effectType, None)

    new(name: string, scopes, effectType, refHint) =
        Effect(StringResource.stringManager.InternIdentifierToken name, scopes, effectType, refHint)

type ScriptedEffect(name: StringTokens, scopes, effectType, comments, globals, settargets, usedtargets) =
    inherit Effect(name, scopes, effectType)
    member val Comments: string = comments
    member val GlobalEventTargets: string list = globals
    member val SavedEventTargets: string list = settargets
    member val UsedEventTargets: string list = usedtargets

    override x.Equals(y) =
        match y with
        | :? ScriptedEffect as y -> x.Name = y.Name && x.Scopes = y.Scopes && x.Type = y.Type
        | _ -> false


    override x.GetHashCode() =
        hash (x.Name, x.Scopes, x.Type, x.Comments, x.GlobalEventTargets, x.SavedEventTargets, x.UsedEventTargets)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.normal.CompareTo(y.Name.normal)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

type DocEffect(name: StringTokens, scopes, target, effectType, desc, usage, refHint) =
    inherit Effect(name, scopes, effectType, refHint)
    member val Desc: string = desc
    member val Usage: string = usage
    member val Target: Scope option = target

    override x.Equals(y) =
        match y with
        | :? DocEffect as y ->
            x.Name = y.Name
            && x.Scopes = y.Scopes
            && x.Type = y.Type
            && x.Desc = y.Desc
            && x.Usage = y.Usage
        | _ -> false

    override x.GetHashCode() =
        hash (x.Name, x.Scopes, x.Type, x.Desc, x.Usage, x.Target)


    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Effect as y -> x.Name.normal.CompareTo(y.Name.normal)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

    new(rawEffect: RawEffect, effectType: EffectType, parseScopes) =
        let scopes =
            rawEffect.scopes |> List.map (fun x -> x.Trim()) |> List.collect parseScopes

        let target = rawEffect.targets |> List.collect parseScopes |> List.tryHead

        DocEffect(
            StringResource.stringManager.InternIdentifierToken rawEffect.name,
            scopes,
            target,
            effectType,
            rawEffect.desc,
            rawEffect.usage
        )

    new(name, scopes, target, effectType, desc, usage) = DocEffect(name, scopes, target, effectType, desc, usage, None)

    new(name: string, scopes, target, effectType, desc, usage) =
        DocEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            target,
            effectType,
            desc,
            usage,
            None
        )

    new(name: string, scopes, target, effectType, desc, usage, refHint) =
        DocEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            target,
            effectType,
            desc,
            usage,
            refHint
        )

type ScopedEffect
    (
        name: StringTokens,
        scopes,
        inner,
        effectType,
        desc,
        usage,
        isScopeChange,
        ignoreChildren,
        scopeonlynoteffect,
        isValue,
        isWildCard,
        refHint
    ) =
    inherit DocEffect(name, scopes, inner, effectType, desc, usage, refHint)
    member val IsScopeChange: bool = isScopeChange
    member val IgnoreChildren: string list = ignoreChildren
    member val ScopeOnlyNotEffect: bool = scopeonlynoteffect
    /// If this scoped effect is a value scope
    member val IsValueScope: bool = isValue
    /// If this scoped effect is a prefix that should accept anything afterwards
    member val IsWildCard: bool = isWildCard

    new(de: DocEffect, inner, isScopeChange, ignoreChildren, scopeonlynoteffect, isValue) =
        ScopedEffect(
            de.Name,
            de.Scopes,
            inner,
            de.Type,
            de.Desc,
            de.Usage,
            isScopeChange,
            ignoreChildren,
            scopeonlynoteffect,
            isValue,
            false,
            None
        )

    new(de: DocEffect, inner) =
        ScopedEffect(de.Name, de.Scopes, inner, de.Type, de.Desc, de.Usage, true, [], false, false, false, None)

    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect, isValue, refHint) =
        ScopedEffect(
            name,
            scopes,
            inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            isValue,
            false,
            refHint
        )

    new(name: string, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(
            StringResource.stringManager.InternIdentifierToken name,
            scopes,
            Some inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            false,
            false,
            None
        )
    // ScopedEffect((StringResource.stringManager.InternIdentifierToken name), scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false, None)
    new(name: StringTokens, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(name, scopes, inner, effectType, desc, usage, true, [], scopeonlynoteffect, false, false, None)

    new(name, scopes, inner, effectType, desc, usage, scopeonlynoteffect) =
        ScopedEffect(
            name,
            scopes,
            Some inner,
            effectType,
            desc,
            usage,
            true,
            [],
            scopeonlynoteffect,
            false,
            false,
            None
        )