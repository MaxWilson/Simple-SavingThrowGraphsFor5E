#if INTERACTIVE
#r "nuget: Thoth.Json.Net"
open Thoth.Json.Net
#else
module Compute
open Thoth.Json
#endif
type Stats = {
    str: int * int option
    dex: int * int option
    con: int * int option
    int: int * int option
    wis: int * int option
    cha: int * int option
    advantage: string
    ac: int
    hp: int
    speed: string
    magicResistance: bool
    dcStr: int option
    dcDex: int option
    dcCon: int option
    dcInt: int option
    dcWis: int option
    dcCha: int option
    skills: (string * int) list
    damageResistances: string
    damageImmunities: string
    damageVuln: string
    conditionImmunities: string list
    }
type Header = {
    name: string
    stats: Stats
    cr: float
    size: string
    creatureType: string
    tags: string list
    alignment: string
    ac: int
    hp: int
    legendary: bool
    unique: bool
    source: string
    sourcebook: string
    }

let mutable creatures: Header array = [||]
let mutable byName = creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
let sources = [|"Out of the Abyss"; "Princes of the Apocalypse";
    "Hoard of the Dragon Queen"; "Rise of Tiamat"; "Monster Manual";
    "Princes of the Apocalypse Online Supplement v1"; "Basic Rules v1";
    "HotDQ supplement"; "Player's Handbook"; "Mordenkainen's Tome of Foes";
    "Storm King's Thunder"; "Curse of Strahd"; "Tales from the Yawning Portal";
    "Tomb of Annihilation"; "The Tortle Package"; "Volo's Guide to Monsters";
    "Eberron - Rising from the Last War"|]
let mutable creatureTypes = [||]
let crs = [0.; 0.125; 0.25; 0.5] @ [1. .. 1. .. 30.]
let mutable byCR = creatures |> Array.groupBy (fun m -> m.cr) |> Map.ofSeq
let mutable bySrcCR = creatures |> Array.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq
let initialize input =
    creatures <- input
    byName <- creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
    byName <- creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
    byCR <- creatures |> Array.groupBy (fun m -> m.cr) |> Map.ofSeq
    bySrcCR <- creatures |> Array.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq
    creatureTypes <- creatures |> Seq.map (fun m -> m.creatureType) |> Seq.distinct |> Seq.filter (not << System.String.IsNullOrWhiteSpace) |> Array.ofSeq

let statsDecoder : Decoder<Stats> =
    let stat = (Decode.tuple2 Decode.int (Decode.option Decode.int))
    Decode.object(fun get -> {
        str = get.Required.At ["str"] stat
        dex = get.Required.At ["dex"] stat
        con = get.Required.At ["con"] stat
        int = get.Required.At ["int"] stat
        wis = get.Required.At ["wis"] stat
        cha = get.Required.At ["cha"] stat
        advantage = get.Optional.At ["advantage"] Decode.string |> Option.defaultValue null
        ac = get.Required.At ["ac"] Decode.int
        hp = get.Required.At ["hp"] Decode.int
        speed = get.Required.At ["speed"] Decode.string
        magicResistance = get.Required.At ["magicResistance"] Decode.bool
        dcStr = get.Optional.At ["dcStr"] Decode.int
        dcDex = get.Optional.At ["dcDex"] Decode.int
        dcCon = get.Optional.At ["dcCon"] Decode.int
        dcInt = get.Optional.At ["dcInt"] Decode.int
        dcWis = get.Optional.At ["dcWis"] Decode.int
        dcCha = get.Optional.At ["dcCha"] Decode.int
        skills = get.Optional.At ["skills"] (Decode.list (Decode.tuple2 Decode.string Decode.int)) |> Option.defaultValue []
        damageResistances = get.Optional.At ["damageResistances"] Decode.string |> Option.defaultValue null
        damageImmunities = get.Optional.At ["damageImmunities"] Decode.string |> Option.defaultValue null
        damageVuln = get.Optional.At ["damageVuln"] Decode.string |> Option.defaultValue null
        conditionImmunities = get.Optional.At ["conditionImmunities"] (Decode.list Decode.string) |> Option.defaultValue []
    })
let headerDecoder : Decoder<Header> =
    Decode.object(fun get -> {
        Header.name = get.Required.At ["name"] Decode.string
        stats = get.Required.At ["stats"] statsDecoder
        cr = get.Required.At ["cr"] Decode.float
        size = get.Required.At ["size"] Decode.string
        creatureType = get.Required.At ["creatureType"] Decode.string
        tags = get.Required.At ["tags"] (Decode.list Decode.string)
        alignment = get.Required.At ["alignment"] Decode.string
        ac = get.Required.At ["ac"] Decode.int
        hp = get.Required.At ["hp"] Decode.int
        legendary = get.Required.At ["legendary"] Decode.bool
        unique = get.Required.At ["unique"] Decode.bool
        source = get.Required.At ["source"] Decode.string
        sourcebook = get.Optional.At ["sourcebook"] Decode.string |> Option.defaultValue null
        })


#if INTERACTIVE
initialize (System.IO.File.ReadAllText(sprintf @"c:\code\saves\public\creatures.json") |> Thoth.Json.Net.Decode.fromString (Decode.array headerDecoder) |> function Ok v -> v)
#endif

type Encounter = (int * Header) list
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type XanatharMethod = BiggestMob | Solo | MedianMob | RandomMob | Mixed
type ConstructionMethod = PureCR | Xanathar of XanatharMethod * Difficulty | DMG of Difficulty | ShiningSword of Difficulty
type ConstructionSettings = {
    sources: string list // allowed sources, e.g. Volo's, MM
    partySize: int
    creatureType: string list
    method: ConstructionMethod
    }
type ConstructEncounter = ConstructionSettings -> (*partyLevel*) float -> (* number of encounters *) int -> Encounter list
type Ability = Str | Dex | Con | Int | Wis | Cha
type DefenseMethod = Save | NonmagicalSave | Check
type Attack = AoE of DefenseMethod * maxTargets: int * maxPercentage: float | SingleTarget of DefenseMethod
type EvaluationLine = Evaluation of name: string * attack: Attack * Ability
type Result = Result of percentage: float * Encounter list
// for PureCR pcLevel actually means monster CR, and can go outside 1-20
type LevelResult = { pcLevel: float; average: Result; best: Result; worst: Result }
type EvaluationResponse = {
    name: string
    attack: Attack
    ability: Ability
    results: LevelResult list
    }
type DCComputer = Fixed of int | Dynamic
type EvaluationSettings = {
    abilities: Ability list
    attackType: DefenseMethod list
    dcComputer: DCComputer
    }
type Evaluate = ConstructEncounter -> ConstructionSettings -> EvaluationSettings -> EvaluationResponse list

let calculateEffectiveness (a: Attack) (ability: Ability) (dc: int) (encounter: Encounter) : float =
    let numberOfMonsters = encounter |> List.sumBy(fun (n, m) -> n)
    let effectivenessOfDefense (m: Header) defense =
        let save (stat, save) =
            match save with
                    | Some v -> v
                    | None -> (stat) / 2 - 5
        let stat (stat, _) = stat
        let ab = match ability with
                    | Str -> m.stats.str
                    | Dex -> m.stats.dex
                    | Con -> m.stats.con
                    | Int -> m.stats.int
                    | Wis -> m.stats.wis
                    | Cha -> m.stats.cha
        let hasAdvantage =
            if m.stats.advantage = null then false
            else
                let adv txt = m.stats.advantage.ToLowerInvariant().Contains(txt)
                match ability with
                    | Str -> adv "strength"
                    | Dex -> adv "dexterity"
                    | Con -> adv "constitution"
                    | Int -> adv "intelligence"
                    | Wis -> adv "wisdom"
                    | Cha -> adv "charisma"
        let success bonus dc =
            min 1. (max 0. (float (dc - bonus) / 20.))
        let sq v = v * v
        match defense with
        | Save when m.stats.magicResistance ->
            (success (save ab) dc) |> sq
        | Save | NonmagicalSave ->
            if hasAdvantage then
                (success (save ab) dc) |> sq
            else
                (success (save ab) dc)
        | Check -> success (stat ab) dc
    let numberAffected = encounter |> List.sumBy(fun (n,m) ->
        match a with
        | SingleTarget d -> effectivenessOfDefense m d
        | AoE(d, maxTargets, maxPct) -> effectivenessOfDefense m d * (min (float n * maxPct/100.) (float maxTargets))
        )
    let effectiveness = numberAffected / (float numberOfMonsters) * 100.
    effectiveness

let dcOf =
    function
    // assume maxed stats + prof
    | Dynamic ->
        function
        | n when n <= 3. -> 13
        | n when n <= 4. -> 14
        | n when n <= 7. -> 15
        | n when n <= 8. -> 16
        | n when n <= 12. -> 17
        | n when n <= 16. -> 18
        | n -> 19
    | Fixed n -> fun _ -> n

let eval: Evaluate = fun construct constructSettings evalSettings ->
    let range = match constructSettings.method with PureCR -> [0.; 0.125; 0.25; 0.5] @ [1. .. 30.] | _ -> [1. .. 20.]
    let N = 1000
    let encountersByLevel = range |> Seq.map(fun level -> level, construct constructSettings level N) |> Map.ofSeq
    let linesOf (settings: EvaluationSettings) =
        [
            for ability in settings.abilities do
                for attackType in settings.attackType do
                    Evaluation(
                        (match attackType with
                            | Save -> ability.ToString()
                            | Check -> sprintf "%A check" ability
                            | NonmagicalSave -> sprintf "%A (bypass MR)" ability),
                        // for now, no AoEs are supported
                        SingleTarget(attackType),
                        ability)

        ]
    [for Evaluation(name, attack, ability) in evalSettings |> linesOf do
        {
        EvaluationResponse.name = name
        attack = attack
        ability = ability
        results = [for level in range do
                    let dc = dcOf evalSettings.dcComputer level

                    let encounters = encountersByLevel.[level]
                    let by f =
                            let c = (calculateEffectiveness attack ability dc)
                            let e = encounters |> f c
                            in Result(c e, [e])
                    if not encounters.IsEmpty then {
                        pcLevel = level
                        average = Result(encounters |> Seq.averageBy (calculateEffectiveness attack ability dc), encounters)
                        best = by Seq.maxBy
                        worst = by Seq.minBy
                        }
                    ]
        }]

let r = System.Random()
let chooseFrom (choices: 't array) = choices.[r.Next choices.Length]
let constructPureCR : ConstructEncounter =
    fun (settings: ConstructionSettings) cr N ->
        let creatures =
            match byCR |> Map.tryFind cr with
            | Some creatures ->
                creatures |> Array.filter (fun (m:Header) ->
                    settings.sources |> List.exists((=) m.sourcebook)
                    && (settings.creatureType.IsEmpty
                        || settings.creatureType |> List.exists((=)m.creatureType)))
            | None -> Array.empty
        // 1 of each creature at each CR, ignoring N
        List.ofArray (creatures |> Array.map (fun m -> [1, m]))

