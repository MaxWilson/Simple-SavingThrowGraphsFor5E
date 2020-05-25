module Compute

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
let crs = [0.; 0.125; 0.25; 0.5] @ [1. .. 1. .. 30.]
let mutable byCR = creatures |> Array.groupBy (fun m -> m.cr) |> Map.ofSeq
let mutable bySrcCR = creatures |> Array.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq
let initialize input =
    creatures <- input
    byName <- creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
    byName <- creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
    byCR <- creatures |> Array.groupBy (fun m -> m.cr) |> Map.ofSeq
    bySrcCR <- creatures |> Array.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq
#if INTERACTIVE
#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json
initialize (System.IO.File.ReadAllText(sprintf @"c:\code\saves\data.json") |> JsonConvert.DeserializeObject<Header array>)
#endif


type Encounter = (int * Header) list
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type XanatharMethod = BiggestMob | Solo | MedianMob | RandomMob | Mixed
type ConstructionMethod = PureCR | Xanathar of XanatharMethod | DMG of Difficulty | Balanced of Difficulty
type ConstructionSettings = {
    sources: string list // allowed sources, e.g. Volo's, MM
    partySize: int
    creatureType: string list
    method: ConstructionMethod
    }
type ConstructEncounter = ConstructionSettings -> (*partyLevel*) float -> (* number of encounters *) int -> Encounter list
type Ability = Str | Dex | Con | Int | Wis | Cha
type DefenseMethod = Save of Ability | NonmagicalSave of Ability | Check of Ability
type Attack = AoE of DefenseMethod * maxTargets: int * maxPercentage: float | SingleTarget of DefenseMethod
type EvaluationLine = Evaluation of name: string * attack: Attack
type Result = Result of percentage: float * Encounter list
// for PureCR pcLevel actually means monster CR, and can go outside 1-20
type LevelResult = { pcLevel: float; average: Result; best: Result; worst: Result }
type EvaluationResponse = {
    name: string
    attack: Attack
    ability: Ability
    results: LevelResult list
    }
type Evaluate = ConstructEncounter -> ConstructionSettings -> EvaluationLine list -> EvaluationResponse list

let eval: Evaluate = fun construct constructSettings evalLines ->
    let range = match constructSettings.method with PureCR -> [0.; 0.125; 0.25; 0.5] @ [1. .. 30.] | _ -> [1. .. 20.]
    let N = 1000
    let encountersByLevel = range |> Seq.map(fun level -> level, construct constructSettings level N) |> Map.ofSeq
    let abilityOfDefense = function Save v | NonmagicalSave v | Check v -> v
    let calculateEffectiveness (a: Attack) (e: Encounter) : float =
        let effectiveness = 50.
        effectiveness
    [for Evaluation(name, attack) in evalLines do
        {
        EvaluationResponse.name = name
        attack = attack
        ability = match attack with AoE(d, _, _) | SingleTarget d -> abilityOfDefense d
        results = [for level in range do
                    let encounters = encountersByLevel.[level]
                    let by f =
                            let c = (calculateEffectiveness attack)
                            let e = encounters |> f c
                            in Result(c e, [e])
                    if not encounters.IsEmpty then {
                        pcLevel = level
                        average = Result(encounters |> Seq.averageBy (calculateEffectiveness attack), encounters)
                        best = by Seq.maxBy
                        worst = by Seq.minBy
                        }
                    ]
        }]

let r = System.Random()
let chooseFrom (choices: 't array) = choices.[r.Next choices.Length]
let constructPureCR : ConstructEncounter =
    fun (settings: ConstructionSettings) cr N ->
        let creatures = byCR.[cr] |> Array.filter (fun (m:Header) -> settings.sources |> List.exists((=) m.sourcebook))
        [for n in 1..N do
            [1, chooseFrom creatures]
            ]
