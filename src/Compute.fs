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

let mutable creatures: Header list = []
#if INTERACTIVE
#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json
creatures <- System.IO.File.ReadAllText(sprintf @"c:\code\saves\data.json") |> JsonConvert.DeserializeObject<Header list>
#endif

let byName = creatures |> Seq.map (fun m -> m.name, m) |> Map.ofSeq
let sources = [|"Out of the Abyss"; "Princes of the Apocalypse";
    "Hoard of the Dragon Queen"; "Rise of Tiamat"; "Monster Manual";
    "Princes of the Apocalypse Online Supplement v1"; "Basic Rules v1";
    "HotDQ supplement"; "Player's Handbook"; "Mordenkainen's Tome of Foes";
    "Storm King's Thunder"; "Curse of Strahd"; "Tales from the Yawning Portal";
    "Tomb of Annihilation"; "The Tortle Package"; "Volo's Guide to Monsters";
    "Eberron - Rising from the Last War"|]
let crs = [0.; 0.125; 0.25; 0.5] @ [1. .. 1. .. 30.]
let bySrcCR = creatures |> List.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq

for cr in crs do
    match bySrcCR |> Map.tryFind ("Tomb of Annihilation", cr) with
    | Some monsters ->
        for m in monsters do
            printfn "%.2g %s" cr m.name
    | None -> ()

type Encounter = (int * Header) list
type Difficulty = Easy | Medium | Hard | Deadly | Ludicrous
type XanatharMethod = BiggestMob | Solo | MedianMob | RandomMob | Mixed
type ConstructionMethod = PureCR | Xanathar of XanatharMethod | DMG of Difficulty | Balanced of Difficulty
type ConstructionSettings = {
    sources: string list // allowed sources, e.g. Volo's, MM
    partySize: int
    partyLevel: int
    creatureType: string list
    method: ConstructionMethod
    }
type ConstructEncounter = ConstructionSettings -> Encounter
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

let eval: Evaluate = fun construct baseConstructSettings evalLines ->
    let range = match baseConstructSettings.method with PureCR -> [0.; 0.125; 0.25; 0.5] @ [1. .. 30.] | _ -> [1. .. 20.]

    failwith "Not impl"