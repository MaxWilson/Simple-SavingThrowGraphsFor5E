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
    legendaryResistance: int option
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
    "Tomb of Annihilation"; "Volo's Guide to Monsters";
    "Eberron - Rising from the Last War"|]
let attackTags = [|
    // BPS is mostly irrelevant for spells, omitting
    //"bludgeoning, piercing, and slashing from nonmagical attacks";
    //"bludgeoning, piercing, and slashing from nonmagical attacks that aren't adamantine";
    //"bludgeoning, piercing, and slashing from nonmagical weapons";
    //"bludgeoning, piercing, and slashing from nonmagical/nonsilver weapons";
    "save for half";
    "acid"; "blinded"; "charmed"; "cold"; "deafened"; "exhaustion"; "fire";
    "frightened"; "grappled"; "lightning"; "necrotic"; "paralyzed";
    "petrified"; "poison"; "poisoned"; "prone"; "psychic"; "restrained";
    "stunned"; "thunder"; "unconscious"|]
let mutable creatureTypes = [||]
let crs = [0.; 0.125; 0.25; 0.5] @ [1. .. 1. .. 30.]
let mutable byCR = creatures |> Array.groupBy (fun m -> m.cr) |> Map.ofSeq
let mutable bySrcCR = creatures |> Array.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq
let initialize input =
    creatures <- input
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
        legendaryResistance = get.Optional.At ["legendaryResistance"] Decode.int
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


type Encounter = (int * Header) list
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type XanatharMethod = Solo | Group | Mixed
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
    attackType: Attack list
    dcComputer: DCComputer
    scaleEffectivenessDownByLegendaryResistance: bool
    }
type Evaluate = ConstructEncounter -> ConstructionSettings -> EvaluationSettings -> EvaluationResponse list
let encounterToString (e:Encounter) = System.String.Join(" and ", e |> List.map (fun (n,m) -> if n = 1 then m.name else sprintf "%d %ss" n m.name))

#if INTERACTIVE
initialize (System.IO.File.ReadAllText(sprintf @"c:\code\saves\public\creatures.json") |> Thoth.Json.Net.Decode.fromString (Decode.array headerDecoder) |> function Ok v -> v)
fsi.AddPrinter encounterToString
#endif

let save (stat, save) =
    match save with
            | Some v -> v
            | None -> (stat) / 2 - 5
let stat (stat, _) = stat / 2 - 5
let abilityOf m = function
    | Str -> m.stats.str
    | Dex -> m.stats.dex
    | Con -> m.stats.con
    | Int -> m.stats.int
    | Wis -> m.stats.wis
    | Cha -> m.stats.cha
let hasAdvantage (defense:DefenseMethod) (ability: Ability) tags (m: Header) =
    if m.stats.advantage = null then (defense = Save && m.stats.magicResistance)
    else
        let adv txt = m.stats.advantage.ToLowerInvariant().Contains(txt)
        (match ability with
            | Str -> adv "strength"
            | Dex -> adv "dexterity"
            | Con -> adv "constitution"
            | Int -> adv "intelligence"
            | Wis -> adv "wisdom"
            | Cha -> adv "charisma")
        || (defense = Save && m.stats.magicResistance)
        || tags |> List.exists (fun tag -> m.stats.advantage.Contains tag)

let calculateEffectiveness scaleByLR (a: Attack, tags: string list)(ability: Ability) (dc: int) (encounter: Encounter) : int option * float =
    let numberOfMonsters = encounter |> List.sumBy(fun (n, m) -> n)
    let exists = System.String.IsNullOrWhiteSpace >> not
    let effectivenessOfAttack (m: Header) defense =
        if tags |> List.exists(fun tag -> exists m.stats.damageImmunities && m.stats.damageImmunities.Contains tag || m.stats.conditionImmunities <> Unchecked.defaultof<_> && m.stats.conditionImmunities |> List.contains tag) then 0.
        else
            let success bonus dc =
                min 1. (max 0. (float (dc - bonus) / 20.))
            let sq v = v * v
            let mutable effectiveness =
                match defense with
                | Save | NonmagicalSave ->
                    let baseRate = (success (abilityOf m ability |> save) dc)
                    let effectiveness = if hasAdvantage defense ability tags m then sq baseRate else baseRate
                    if scaleByLR = false then effectiveness
                    else
                        match m.stats.legendaryResistance with
                        | Some n -> (effectiveness / (float n + 1.))
                        | None -> effectiveness
                | Check -> success (abilityOf m ability |> stat) dc
            if tags |> List.exists ((=) "save for half") then
                // even failures count for half
                effectiveness <- (1. - effectiveness)/2. + effectiveness
            // now apply resistances, and vulnerabilities
            if m.stats.damageResistances |> exists && tags |> List.exists(fun tag -> m.stats.damageResistances.Contains tag) then
                effectiveness <- effectiveness/2.
            if m.stats.damageVuln |> exists && tags |> List.exists(fun tag -> m.stats.damageVuln.Contains tag) then
                effectiveness <- effectiveness * 2.
            effectiveness
    let numberOfTargets, numberAffected =
        match a with
        | SingleTarget d -> None, encounter |> List.map(fun (n,m) -> effectivenessOfAttack m d) |> List.max
        | AoE(d, maxTargets, maxPct) ->
            let enemies = encounter |> List.collect(fun (n,m) -> let e = effectivenessOfAttack m d in List.init n (fun _ -> e))
            if enemies.Length = 1 then None, enemies.Head
            else
                let nTargets = max 1 (min (float enemies.Length * maxPct/100. |> int) maxTargets) // it should never drop to zero
                let successCount =
                    enemies |> List.sortDescending |> List.take nTargets
                    |> List.sum
                Some nTargets, successCount
    let effectiveness = numberAffected / (float numberOfMonsters) * 100.
    numberOfTargets, effectiveness

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
    let N = 20
    let encountersByLevel = range |> Seq.map(fun level -> level, construct constructSettings level N) |> Map.ofSeq
    let linesOf (settings: EvaluationSettings) =
        [
            for ability in settings.abilities do
                for attackType in settings.attackType do
                    Evaluation(
                        (match (attackType |> function SingleTarget(d) | AoE(d,_,_) -> d) with
                            | Save -> ability.ToString()
                            | Check -> sprintf "%A check" ability
                            | NonmagicalSave -> sprintf "%A (bypass MR)" ability),
                        // for now, no AoEs are generated
                        attackType,
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
                            let calc e = (calculateEffectiveness evalSettings.scaleEffectivenessDownByLegendaryResistance (attack,[]) ability dc e)
                            let e = encounters |> f (calc >> snd)
                            in Result(calc e |> snd, [e])
                    if not encounters.IsEmpty then {
                        pcLevel = level
                        average = Result(encounters |> Seq.averageBy (calculateEffectiveness evalSettings.scaleEffectivenessDownByLegendaryResistance (attack,[]) ability dc >> snd), encounters)
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

module Xanathar =
    // deliberately skipping over CR 0 because they are boring encounters
    let crs = [0.125; 0.25; 0.5] @ [1. .. 30.] |> Array.ofList
    let row start values =
        let mk ratios =
            let ratios = Array.ofList ratios
            let hds = crs |> Array.take (Array.length ratios)
            let m = Array.zip hds ratios |> Map.ofArray
            fun cr -> m |> Map.tryFind cr
        match start with
        | (1,8) -> [1,12]@values |> mk
        | (1,1) -> [1,12;1,12;1,12;1,12]@values |> mk
        | (2,1) -> [1,12;1,12;1,12;1,12;1,12]@values |> mk
        | _ -> fun _ -> None
    let groupRatios =
        let rows = Map.ofList [
                1, row (1,8) [1,2; 1,1; 3,1; 5,1]
                2, row (1,8) [1,3; 1,2; 1,1; 3,1; 6,1]
                3, row (1,8) [1,5; 1,2; 1,1; 2,1; 4,1; 6,1]
                4, row (1,8) [1,8; 1,4; 1,2; 1,1; 2,1; 4,1; 6,1]
                5, row (1,8) [1,12; 1,8; 1,4; 1,2; 1,1; 2,1; 3,1; 5,1; 6,1]
                6, row (1,8) [1,12; 1,9; 1,5; 1,2; 1,1; 2,1; 2,1; 4,1; 5,1; 6,1]
                7, row (1,8) [1,12; 1,12; 1,6; 1,3; 1,1; 1,1; 2,1; 3,1; 4,1; 5,1]
                8, row (1,8) [1,12; 1,12; 1,7; 1,4; 1,2; 1,1; 2,1; 3,1; 3,1; 4,1; 6,1]
                9, row (1,8) [1,12; 1,12; 1,8; 1,4; 1,2; 1,1; 1,1; 2,1; 3,1; 4,1; 5,1; 6,1]
                10, row (1,8) [1,12; 1,12; 1,10; 1,5; 1,2; 1,1; 1,1; 2,1; 2,1; 3,1; 4,1; 5,1; 6,1]
                11, row (1,1) [1,6; 1,3; 1,2; 1,1; 2,1; 2,1; 2,1; 3,1; 4,1; 5,1; 6,1]
                12, row (1,1) [1,8; 1,3; 1,2; 1,1; 1,1; 2,1; 2,1; 3,1; 3,1; 4,1; 5,1; 6,1]
                13, row (1,1) [1,9; 1,4; 1,2; 1,2; 1,1; 1,1; 2,1; 2,1; 3,1; 3,1; 4,1; 5,1; 6,1]
                14, row (1,1) [1,10; 1,4; 1,3; 1,2; 1,1; 1,1; 2,1; 2,1; 3,1; 3,1; 4,1; 4,1; 5,1; 6,1]
                15, row (1,1) [1,12; 1,5; 1,3; 1,2; 1,1; 1,1; 1,1; 2,1; 2,1; 3,1; 3,1; 4,1; 5,1; 5,1; 6,1]
                16, row (2,1) [1,5; 1,3; 1,2; 1,1; 1,1; 1,1; 2,1; 2,1; 2,1; 3,1; 4,1; 4,1; 5,1; 5,1; 6,1]
                17, row (2,1) [1,7; 1,4; 1,3; 1,2; 1,1; 1,1; 1,1; 2,1; 2,1; 2,1; 3,1; 3,1; 4,1; 4,1; 5,1; 6,1]
                18, row (2,1) [1,7; 1,5; 1,3; 1,2; 1,1; 1,1; 1,1; 2,1; 2,1; 2,1; 3,1; 3,1; 4,1; 4,1; 5,1; 6,1; 6,1]
                19, row (2,1) [1,8; 1,5; 1,3; 1,2; 1,2; 1,1; 1,1; 1,1; 2,1; 2,1; 2,1; 3,1; 3,1; 4,1; 4,1; 5,1; 6,1; 6,1]
                20, row (2,1) [1,9; 1,6; 1,4; 1,2; 1,2; 1,1; 1,1; 1,1; 1,1; 2,1; 2,1; 2,1; 3,1; 3,1; 4,1; 4,1; 5,1; 5,1; 6,1]
            ]
        fun lvl cr ->
            rows |> Map.tryFind lvl |> Option.bind (fun m -> m cr)
    let soloMax =
        [
            2,2,1
            4,3,2
            5,4,3
            6,5,4
            9,8,7
            10,9,8
            11,10,9
            12,11,10
            13,12,11
            14,13,12
            15,14,13
            17,16,15
            18,17,16
            19,18,17
            20,19,18
            21,20,19
            22,21,20
            22,21,20
            23,22,21
            24,23,22
        ]
        |> List.mapi (fun ix (six, five, four) -> (ix+1), {| six = six; five = five; four = four |})
        |> Map.ofSeq

module DMGish =
    open System.Math
    type Advancement = { level: int; XPReq: int; proficiencyBonus: int }
    let levelAdvancement =
        [|
        // Level, XP required, proficiency bonus
        1, 0, +2
        2, 300, +2
        3, 900, +2
        4, 2700, +2
        5, 6500, +3
        6, 14000, +3
        7, 23000, +3
        8, 34000, +3
        9, 48000, +4
        10, 64000, +4
        11, 85000, +4
        12, 100000, +4
        13, 120000, +5
        14, 140000, +5
        15, 165000, +5
        16, 195000, +5
        17, 225000, +6
        18, 265000, +6
        19, 305000, +6
        20, 355000, +6
        |] |> Array.map (fun (level, xpReq, proficiencyBonus) -> { level = level; XPReq = xpReq; proficiencyBonus = proficiencyBonus })

    type XPBudget = { level: int; easy: int; medium: int; hard: int; deadly: int; daily: int }
    let xpBudgets =
        [|
        // Level, easy, medium, hard, deadly, daily
        1, 25, 50, 75, 100, 300
        2, 50, 100, 150, 200, 600
        3, 75, 150, 225, 400, 1200
        4, 125, 250, 375, 500, 1700
        5, 250, 500, 750, 1100, 3500
        6, 300, 600, 900, 1400, 4000
        7, 350, 750, 1100, 1700, 5000
        8, 450, 900, 1400, 2100, 6000
        9, 550, 1100, 1600, 2400, 7500
        10, 600, 1200, 1900, 2800, 9000
        11, 800, 1600, 2400, 3600, 10500
        12, 1000, 2000, 3000, 4500, 11500
        13, 1100, 2200, 3400, 5100, 13500
        14, 1250, 2500, 3800, 5700, 15000
        15, 1400, 2800, 4300, 6400, 18000
        16, 1600, 3200, 4800, 7200, 20000
        17, 2000, 3900, 5900, 8800, 25000
        18, 2100, 4200, 6300, 9500, 27000
        19, 2400, 4900, 7300, 10900, 30000
        20, 2800, 5700, 8500, 12700, 40000
        |] |> Array.map (fun (level, easy, medium, hard, deadly, daily) -> { level = level; easy = easy; medium = medium; hard = hard; deadly = deadly; daily = daily})
    type MonsterCR = { CR: float; XPReward: int }
    let monsterCR =
        [|
        // CR, XP
        0., 10
        0.125, 25
        0.25, 50
        0.5, 100
        1., 200
        2., 450
        3., 700
        4., 1100
        5., 1800
        6., 2300
        7., 2900
        8., 3900
        9., 5000
        10., 5900
        11., 7200
        12., 8400
        13., 10000
        14., 11500
        15., 13000
        16., 15000
        17., 18000
        18., 20000
        19., 22000
        20., 25000
        21., 33000
        22., 41000
        23., 50000
        24., 62000
        25., 75000
        26., 90000
        27., 105000
        28., 120000
        29., 135000
        30., 155000
        |] |> Array.map (fun (cr, xp) -> { CR = cr; XPReward = xp })
    let xpValue =
        let xps = monsterCR |> Array.map(fun { CR = cr; XPReward = xp } -> cr, xp) |> Map.ofArray
        fun cr -> xps.[cr]

    let sscalculate (monsters: Header seq) =
        let costs = monsters |> Seq.map (fun m -> Pow((xpValue m.cr |> float) / 100., (2./3.)))
        (Pow(Seq.sum costs, 1.5) * 100. |> Round |> int)
    let dmgcalculate partySize (monsters: Header seq) =
        let costs = monsters |> Seq.sumBy (fun m -> xpValue m.cr)
        let mult =
            let mults = [0.5; 1.; 1.5; 2.; 2.5; 3.; 4.; 5.]
            let ix =
                match monsters |> Seq.length with
                | 1 -> 1
                | 2 -> 2
                | n when n <= 6 -> 3
                | n when n <= 10 -> 4
                | n when n <= 14 -> 5
                | _ -> 6
            match partySize with
            | n when n < 3 -> mults.[ix+1]
            | n when n <= 6 -> mults.[ix]
            | _ -> mults.[ix-1]
        (float costs) * mult |> Round |> int

    let makeEncounter allowedCreatures calculate (xpMin: int, xpMax: int) : Encounter =
        let newTemplate() =
            // pick two or three types of creatures from allowedCreatures
            if r.Next(100) <= 50 then
                Array.init 3 (fun _ -> chooseFrom allowedCreatures)
            else
                Array.init 2 (fun _ -> chooseFrom allowedCreatures)
        let rec generate template =
            let rec addMonster monsters =
                let precost = calculate monsters
                let monster = chooseFrom template
                let monsters' = monster::monsters
                let postcost = calculate monsters'
                if postcost <= xpMax then
                    addMonster monsters' // keep going!
                else
                    if postcost > xpMax && precost < xpMin then
                        // it's too cheap before and too expensive after, no legal way to use this candidate
                        // send signal to start over with a new template instead
                        []
                    elif precost >= xpMin then
                        monsters
                    else
                        // too expensive
                        []
            match addMonster [] with
            | [] ->
                generate(newTemplate()) // this template was too tough to allow even one monster--choose a different template
            | candidate ->
                // in order to avoid bias towards barely-difficult-enough encounters,
                // we'll make the probability of accepting a candidate proportional to how much of the (max-min) range it occupies
                let cost = calculate candidate
                let range = xpMax - xpMin
                let fraction = float (cost - xpMin) / float range
                if r.NextDouble() <= fraction then candidate
                else generate(newTemplate())
        let toEncounter (monsters: Header list) : Encounter =
            monsters |> List.groupBy id |> List.map(fun (m, lst) -> List.length lst, m)
        generate(newTemplate()) |> toEncounter

let buildEncounter: ConstructEncounter =
    fun settings lvl N ->
        let encounters =
            match settings.method with
            | PureCR -> constructPureCR settings lvl N
            | Xanathar(typ, diff) ->
                let lvl = int lvl
                let creature =
                    let creatures = byCR |> Map.map(fun lvl creatures ->
                        creatures |> Array.filter (fun (m:Header) ->
                            settings.sources |> List.exists((=) m.sourcebook)
                            && (settings.creatureType.IsEmpty
                                || settings.creatureType |> List.exists((=)m.creatureType))))
                    fun cr ->
                        match creatures.[cr] with
                        | cs when cs.Length > 0 -> cs |> chooseFrom |> Some
                        | _ -> None
                let rec generateGroupEncounter remainingBudget attempts: Encounter =
                    let candidateCRs = (Xanathar.crs |> Array.filter (fun cr ->
                        match Xanathar.groupRatios lvl cr with
                        | Some(n,m) -> (float n)/(float m) <= remainingBudget
                        | _ -> false))
                    // if there are no eligible monsters, there are no eligible monsters.
                    if candidateCRs.Length = 0 || attempts > 50 then []
                    else
                        let cr = chooseFrom candidateCRs
                        let (monsterRatio, pcRatio) = Xanathar.groupRatios lvl cr |> Option.get
                        let maxMonsters = remainingBudget * (float pcRatio) / (float monsterRatio) |> System.Math.Round |> int
                        let numberOfMonsters =
                            match r.Next 100 with
                            | n when n <= 50 -> maxMonsters
                            | _ -> 1 + r.Next(maxMonsters)
                        match creature cr with
                        | Some creature ->
                            let cost = (float numberOfMonsters * float monsterRatio) / (float pcRatio)
                            (numberOfMonsters, creature)::(generateGroupEncounter (remainingBudget - cost) 1)
                        | None -> // pick something else
                            (generateGroupEncounter (remainingBudget) (attempts+1))
                let rec generateSolo cr : Encounter =
                    match creature (float cr) with
                    | Some creature -> [1, creature]
                    | None -> // no solo creatures at this level, try next lower
                        if(cr - 1) > 0 then
                            generateSolo (cr - 1)
                        else []
                let group =
                    let budget = float settings.partySize * match diff with Easy -> (2./3.) | Medium -> 1.0 | (Hard | _) -> 1.5
                    fun _ -> generateGroupEncounter budget 1
                let solo =
                    let soloBudget = Xanathar.soloMax.[lvl]
                    let crMax =
                        if settings.partySize <= 4 then soloBudget.four
                        elif settings.partySize >= 6 then soloBudget.five
                        else soloBudget.five
                    let crMax =
                        match diff with Easy -> crMax - 3 | Medium -> crMax | (Hard | _) -> crMax + 2
                    fun _ -> generateSolo crMax
                match typ with
                | Group -> List.init N group
                | Solo -> List.init N solo
                | Mixed -> List.init N (fun x -> (chooseFrom [|solo; group|]) x)
            | DMG diff | ShiningSword diff ->
                let allowedCreatures =
                    creatures |> Array.filter (fun (m:Header) ->
                        settings.sources |> List.exists((=) m.sourcebook)
                        && (settings.creatureType.IsEmpty
                            || settings.creatureType |> List.exists((=)m.creatureType)))
                let lvlStats = DMGish.xpBudgets |> Array.find (fun d -> d.level = int lvl)
                let xpBounds =
                    let (xpMin, xpMax) =
                        match diff with
                        | Easy -> lvlStats.easy, lvlStats.medium - 1
                        | Medium -> lvlStats.medium, lvlStats.hard - 1
                        | Hard -> lvlStats.hard, lvlStats.deadly - 1
                        | Deadly -> lvlStats.deadly, (lvlStats.deadly * 3) / 2
                        | Ludicrous -> lvlStats.deadly * 2, lvlStats.deadly * 6
                    let (xpMin, xpMax) = settings.partySize * xpMin, settings.partySize * xpMax
                    match settings.method with
                    | ShiningSword _ when settings.partySize <> 4->
                        // Shining Sword scales both PC Power and monster power as the 3/2 power of size
                        // Shining Sword has no XP multiplier like DMG does, so we apply scaling directly to bounds,
                        // treating 4 PCs are the standard case
                        let scalingFactor = System.Math.Pow(float settings.partySize / 4., 2./3.)
                        (float xpMin * scalingFactor |> int), (float xpMax * scalingFactor |> int)
                    | _ -> (xpMin, xpMax)
                let calculate = match settings.method with DMG _ -> DMGish.dmgcalculate settings.partySize | _ -> DMGish.sscalculate
                List.init N (fun _ ->
                    DMGish.makeEncounter allowedCreatures calculate xpBounds)
        encounters |> List.distinct // no point in showing duplicates on the graphs
