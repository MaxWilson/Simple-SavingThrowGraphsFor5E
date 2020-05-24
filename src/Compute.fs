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
let bySrcCR = creatures |> Seq.groupBy (fun m -> m.sourcebook, m.cr) |> Map.ofSeq

for cr in crs do
    match bySrcCR |> Map.tryFind ("Tomb of Annihilation", cr) with
    | Some monsters ->
        for m in monsters do
            printfn "%.2g %s" cr m.name
    | None -> ()
