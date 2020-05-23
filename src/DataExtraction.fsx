open System
open System.IO
open System.Text.RegularExpressions
let files = System.IO.Directory.EnumerateFiles """c:\code\bestiary\_creatures"""
let (|RE|_|) pattern str =
    match Regex.Match(str, pattern, RegexOptions.IgnoreCase) with
    | m when m.Success -> [for x in m.Groups -> x.Value] |> List.filter (not << System.String.IsNullOrWhiteSpace) |> List.tail|> Some
    | _ -> None

type ParseErr = { line: string; err: string }
type Creature = { name: string; txt: string; mutable saves: (string * int) list }
let ptags = "tags:\s*\[([\w\s ,]+)"
let pname = "name:\s*\"([\w '()\-,'/]+)\""
let creatures = files |> Seq.map System.IO.File.ReadAllText |> Seq.map (function RE pname [name] as txt -> name, txt | txt -> printfn "%s" txt; failwithf "No match for %s" (txt.Substring(0, 50))) |>  Map.ofSeq

let getSaves name (txt: string) =
    let lines = txt.Split('\n')

    [for line in txt.Split '\n' do
        match line with
        | RE "DC\s+([0-9]+)\s+(Strength|Dexterity|Constitution|Intelligence|Wisdom|Charisma) saving throw" [dc;st] -> st, System.Int32.Parse dc
        | RE "DC\s+([0-9]+)\s+(Strength|Dexterity|Constitution|Intelligence|Wisdom|Charisma) savng throw" [dc;st] -> st, System.Int32.Parse dc
        | RE "DC\s+[0-9]+\s+.+" [st] -> printfn "Unexpected: %s \n\t\tfor %s" st line
        | _ -> ()
        ]
let mutable annotations = creatures |> Map.map (fun name txt -> { name = name; txt = txt; saves = getSaves name txt })
annotations.["Bulette"].saves <- ["Strength", 16; "Dexterity", 16]
type Row = {
    name: string
    cr: string
    size: string 
    creatureType: string 
    src: string
    str: int * int option
    dex: int * int option
    con: int * int option
    int: int * int option
    wis: int * int option
    cha: int * int option
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

let parseRow a =
    let parseInt v = 
        match v with
        | RE "([0-9 ]+)" [raw] ->    
            match System.Int32.TryParse raw with
            | true, n -> n
            | _ -> failwithf "'%s' is not an int (%s)" v (a:Creature).name
        | v -> failwithf "'%s' is not an int (%s)" v a.name
    let getSaveDC name =
        match a.saves |> List.choose (fun (label, dc) -> if label = name then Some dc else None) with
        | [] -> None
        | saves -> Some(List.max saves)
    let tags =
        match a.txt with
        | RE ptags [tags] -> tags
        | _ -> ""
    let cr =
        match a.txt with
        | RE "Challenge:\s+\"([0-9]+)" [cr] -> "CR " + cr
        | _ -> ""
    let tl = tags.Split ',' |> Seq.map (fun x -> x.Trim()) |> List.ofSeq
    let size = tl.[0]
    let type' = tl.[1]
    let src = if tl.Length > 3 then tl.[3] else ""
    let stat name = 
        let pattern = ("\n" + name + ": ([0-9]+)")
        match a.txt with 
        | RE pattern [raw] ->   
            let raw = parseInt raw
            let modifier = (raw + 1)/2 - 5
            match a.txt with
            | RE "saving_throws:[^\r](.*)" [sts] ->
                let pattern = name + "\s*([+0-9 ]+)"
                match sts with
                | RE pattern [save] -> 
                    raw, Some (parseInt (save.Replace(" ", "")))
                | _ -> raw, None
            | _ -> raw, None
        | _ -> failwithf "Couldn't parse %s '%s'" name (a.txt.Substring(0, 100))
    let intStat name =
        let pattern = sprintf "\n%s: \"([^\"\r]+)\"" name
        match a.txt with
        | RE pattern [v] -> parseInt (v.Replace(" ", ""))
        | _ -> failwithf "Couldn't parse %s '%s'" name (a.txt.Substring(0, 100))
    let strStat name =
        let pattern = sprintf "\n%s: \"([^\"\r]+)\"" name
        match a.txt with
        | RE pattern [v] -> v
        | _ -> null
    let listStat name =
        let pattern = sprintf "\n%s: \"([^\"\r]+)\"" name
        match a.txt with
        | RE pattern [v] -> v.Split(',') |> Seq.map(fun v -> v.Trim()) |> List.ofSeq
        | _ -> []
    let listIntStat name =
        let pattern = sprintf "\n%s: \"([^\"\r]+)\"" name
        match a.txt with
        | RE pattern [v] ->
            v.Split(',') |> Seq.choose(fun x -> 
                match x.Trim() with
                | RE "([a-zA-Z ]+)\s([+ \-0-9]+)" [stat; plus] -> Some(stat.Trim(), parseInt (plus.Replace(" ", "").Trim()))
                | "plus one of the following" -> None
                | _ -> failwithf "Could not parse '%s' as a skill" v)     
            |> List.ofSeq
        | _ -> []
    let str = stat "str"
    let dex = match a.txt with RE "dex: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let con = match a.txt with RE "con: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let int = match a.txt with RE "int: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let wis = match a.txt with RE "wis: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let cha = match a.txt with RE "cha: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    {
        name = a.name
        cr = cr
        size = size 
        creatureType = type' 
        src = src 
        str = (stat "str") 
        dex = (stat "dex") 
        con = (stat "con") 
        int = (stat "int") 
        wis = (stat "wis") 
        cha = (stat "cha") 
        ac = (intStat "armor_class") 
        hp = (intStat "hit_points") 
        magicResistance = match a.txt with RE "(Magic Resistance)" [_] -> true | _ -> false 
        speed = (strStat "speed") 
        dcStr = (getSaveDC "Strength") 
        dcDex = (getSaveDC "Dexterity") 
        dcCon = (getSaveDC "Constitution") 
        dcInt = (getSaveDC "Intelligence") 
        dcWis = (getSaveDC "Wisdom") 
        dcCha = (getSaveDC "Charisma") 
        skills = (listIntStat "skills")
        conditionImmunities = listStat "condition_immunities"
        damageImmunities = strStat "damage_immunities"
        damageResistances = strStat "damage_resistances"
        damageVuln = strStat "damage_vulnerabilities"
        }
// For entries like "Demonic Boon Template" we don't care if they can't be parsed, they won't be used anyway
let all = annotations |> Seq.map (fun (KeyValue(_, notes)) -> notes) |> Seq.sortBy (fun a -> a.name) |> Seq.choose (fun x -> try parseRow x |> Some with _ when x.name.Contains "Template" -> None) |> Array.ofSeq

// have to leave this part commented out because it slows down VS intellisense perf something awful, but this is how you load Newtonsoft.Json
(*
#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json
*)

System.IO.File.WriteAllText(@"c:\code\saves\annotations.json", all |> JsonConvert.SerializeObject)

type KoboldRow = {
    name: string
    mutable stats: Row option
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
    }

let readField (str: string) startIx =
    let mutable startIx = startIx
    while str.[startIx] = ' ' do startIx <- startIx + 1
    if str.[startIx] = '"' then
        let mutable endIx = startIx + 1
        while str.[endIx] <> '"' do endIx <- endIx + 1
        let v = str.Substring(startIx + 1, endIx - startIx - 1).Trim()
        v, endIx+2
    else
        let mutable endIx = startIx
        while endIx < str.Length && str.[endIx] <> ',' do endIx <- endIx + 1
        let v = str.Substring(startIx, endIx - startIx).Trim()
        v, endIx+1
let readFields (str: string) = 
    let str = str.Replace("\"\"", "'") // don't want to deal with escaped quotes
    let rec loop acc ix =
        if ix >= str.Length then acc
        else
            let v, ix = readField str ix
            loop (acc@[v]) ix
    loop [] 0
let parseInt v = 
        match v with
        | RE "([0-9 ]+)" [raw] ->    
            match System.Int32.TryParse raw with
            | true, n -> n
            | _ -> failwithf "'%s' is not an integer" v
        | v -> failwithf "'%s' is not an integer" v
    
let loadKobold fileName =
    let csv = System.IO.File.ReadAllLines fileName |> Seq.skip 1 |> List.ofSeq
    [for line in csv do
        printfn "%s" line
        let [_;_;name;cr;size;type';tags;_;align;_;ac;hp;_;_;legendary;unique;src] = readFields line
        {
            KoboldRow.name = name
            stats = None
            cr = match cr with "1/4" -> 0.25 | "1/2" -> 0.5 | "1/8" -> 0.125 | _ -> System.Double.Parse cr
            size = size
            creatureType = type'
            tags = tags.Split(',') |> Seq.map (fun s -> s.Trim()) |> List.ofSeq
            alignment = align
            ac = parseInt ac
            hp = parseInt hp
            legendary = not <| System.String.IsNullOrWhiteSpace legendary
            unique = not <| System.String.IsNullOrWhiteSpace unique
            source = src
            }
        ]
// for some reason Rising from the Last War has fewer fields
let loadExt fileName =
    let csv = System.IO.File.ReadAllLines fileName |> Seq.skip 1 |> List.ofSeq
    [for line in csv do
        printfn "%s" line
        let [_;name;cr;size;type';tags;_;align;_;ac;hp;_;_;legendary;unique;src] = readFields line
        {
            KoboldRow.name = name
            stats = None
            cr = match cr with "1/4" -> 0.25 | "1/2" -> 0.5 | "1/8" -> 0.125 | _ -> System.Double.Parse cr
            size = size
            creatureType = type'
            tags = tags.Split(',') |> Seq.map (fun s -> s.Trim()) |> List.ofSeq
            alignment = align
            ac = parseInt ac
            hp = parseInt hp
            legendary = not <| System.String.IsNullOrWhiteSpace legendary
            unique = not <| System.String.IsNullOrWhiteSpace unique
            source = src
            }
        ]

// it turns out to be convenient to define a mutable variable for the data we're working on
let mutable allMonsters = 
    loadKobold "c:\code\saves\kfc official - Monsters.csv"    
    @ loadExt "c:\code\saves\kfc eberron last war - Monsters.csv"    

let save () =
    System.IO.File.WriteAllText(sprintf @"c:\code\saves\data.json", JsonConvert.SerializeObject allMonsters)
let load () =
    allMonsters <- System.IO.File.ReadAllText(sprintf @"c:\code\saves\data.json") |> JsonConvert.DeserializeObject<KoboldRow list>

let mutable currentRow = allMonsters.Head
let correct name src =
    if not <| annotations.ContainsKey name then
        annotations <- annotations |> Map.add name (annotations.[src])
correct "Mind Flayer Lich (Illithilich)" "Illithilich"
correct "Pidlewick II" "Pidlwick II"
correct "Burrow Shark" "Burrowshark"
correct "Iymrith the Dragon" "Iymrith, Ancient Blue Dragon"
correct "Cow (Ox)" "Ox"
annotations <- annotations |> Map.add "Demilich (Acererak)" (let d = annotations.["Demilich"] in { d with saves = ("Charisma", 19)::d.saves })
annotations <- annotations |> Map.add "Demilich (Acererak in lair)" (let d = annotations.["Demilich"] in { d with saves = ("Charisma", 19)::d.saves })

allMonsters |> Seq.find (fun m -> m.name = "Demilich (in lair)")

// correct some mangled or misleading names
allMonsters <- allMonsters |> List.map (function m when m.name = "OgrÃ©moch" -> { m with name = "Ogremoch" } | m when m.name = "Hook Horror Servant" -> { m with name = "Hook Horror Spore Servant" } | m -> m)

// exclude dumb low-level campaign-specific unique NPCs while retaining unique high-level monsters that might actually show up in play
allMonsters <- allMonsters |> List.filter (fun m -> not (m.unique && m.cr <= 8.)) 

let toCr = function
    | 0.125 -> "1/8"
    | 0.25 -> "1/4"
    | 0.5 -> "1/2"
    | n -> n.ToString()
for m in allMonsters |> Seq.filter (fun m -> m.stats.IsNone) do
    if m.stats.IsNone then
        currentRow <- m
        let row = match annotations.TryFind(m.name) with
                    | Some row -> Some row
                    | None ->
                        annotations |> Seq.tryPick (fun (KeyValue(name, data)) -> if name.StartsWith(Regex.Replace(m.name, " \((in lair|coven)\)", ""), System.StringComparison.InvariantCultureIgnoreCase) then Some data else None)
        match row with
        | Some row ->
            try
                m.stats <- { (row |> parseRow) with cr = toCr m.cr } |> Some
                save()
                printfn "%s OK" m.name
            with _ -> printfn "Could not parse '%s' (%s)" m.name m.source
        | None ->
            printfn "supplyStats (\"%s\", strdexconintwischa, [], ac, hp, \"30 ft.\", [], [], []) // %s" m.name m.source
        //

// remove monsters that aren't real monsters deserving of a separate entry
save()

type SavingThrow = Str of int | Dex of int | Con of int | Int of int | Wis of int | Cha of int | MR
type Resist = Vuln of string | Resist of string | Immune of string | Conditions of string list
let supplyStats args =
    let (
                name: string,
                str: int, 
                dex: int,
                con: int,
                int: int, 
                wis: int,
                cha: int,
                saves: SavingThrow list,
                ac: int,
                hp: int,
                speed: string,
                dc: SavingThrow list,
                skills: (string * int) list,
                resists: Resist list) = args
    let m = allMonsters |> Seq.find (fun m -> m.name = name)
    if m.stats.IsNone then
        m.stats <- Some {
            Row.name = name
            Row.cr = toCr m.cr
            Row.src = m.source
            Row.size = m.size
            Row.creatureType = m.creatureType
            str = str, saves |> List.tryPick(function Str x -> Some x | _ -> None)
            dex = dex, saves |> List.tryPick(function Dex x -> Some x | _ -> None)
            con = con, saves |> List.tryPick(function Con x -> Some x | _ -> None)
            int = int, saves |> List.tryPick(function Int x -> Some x | _ -> None)
            wis = wis, saves |> List.tryPick(function Wis x -> Some x | _ -> None)
            cha = cha, saves |> List.tryPick(function Cha x -> Some x | _ -> None)
            ac = ac
            hp = hp
            magicResistance = saves |> List.exists(function MR -> true | _ -> false)
            speed = speed
            dcStr = dc |> List.tryPick(function Str x -> Some x | _ -> None)
            dcDex = dc |> List.tryPick(function Dex x -> Some x | _ -> None)
            dcCon = dc |> List.tryPick(function Con x -> Some x | _ -> None)
            dcInt = dc |> List.tryPick(function Int x -> Some x | _ -> None)
            dcWis = dc |> List.tryPick(function Wis x -> Some x | _ -> None)
            dcCha = dc |> List.tryPick(function Cha x -> Some x | _ -> None)
            skills = skills
            damageVuln = resists |> List.tryPick(function Vuln x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            damageResistances = resists |> List.tryPick(function Resist x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            damageImmunities = resists |> List.tryPick(function Immune x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            conditionImmunities = resists |> List.tryPick(function Conditions x -> Some x | _ -> None) |> (function Some x -> x | _ -> [])
            }

supplyStats ("Giant Ice Toad", 16, 13, 14, 8, 10, 6, [], 14, 52, "30 ft.", [], [], []) // Tales from the Yawning Portal: 235
supplyStats ("Giant Skeleton", 21, 10, 20, 4, 6, 6, [], 17, 115, "30 ft.", [], [], [Vuln "bludgeoning"; Immune "poison"; Conditions ["exhaustion"; "poisoned"]]) // Tales from the Yawning Portal: 236
supplyStats ("Thayan Apprentice", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Tales from the Yawning Portal: 245
supplyStats ("Acererak", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Tomb of Annihilation: 209
supplyStats ("Tabaxi Ministrel", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Tomb of Annihilation: 233
supplyStats ("Decapus", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // The Tortle Package: 21
supplyStats ("Geonid", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // The Tortle Package: 22
supplyStats ("Topi", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // The Tortle Package: 22
supplyStats ("Belashyrra", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 286
supplyStats ("Dyrrn", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 288
supplyStats ("Clawfoot", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 289
supplyStats ("Fastieth", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 289
supplyStats ("Dolgaunt", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 290
supplyStats ("Dolgrim", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 291
supplyStats ("Dusk Hag", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 292
supplyStats ("Expeditious Messenger", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 293
supplyStats ("Iron Defender", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 293
supplyStats ("Inspired", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 294
supplyStats ("Karrnathi Undead Soldier", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 295
supplyStats ("Lady Illmarrow", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 297
supplyStats ("Living Burning Hands", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 298
supplyStats ("Living Lightning Bolt", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 299
supplyStats ("Living Cloudkill", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 299
supplyStats ("The Lord of Blades", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 300
supplyStats ("Mordakhesh", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 301
supplyStats ("Rak Tulkhesh", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 303
supplyStats ("Sul Khatesh", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 304
supplyStats ("Hashalaq Quori", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 305
supplyStats ("Kalaraq Quori", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 306
supplyStats ("Tsucora Quori", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 307
supplyStats ("Radiant Idol", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 308
supplyStats ("Zakya Rakshasa", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 309
supplyStats ("Undying Councilor", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 311
supplyStats ("Undying Soldier", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 311
supplyStats ("Valenar Hawk", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 312
supplyStats ("Valenar Hound", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 312
supplyStats ("Valenar Steed", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 313
supplyStats ("Warforged Colossus", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 314
supplyStats ("Warforged Titan", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 315
supplyStats ("Bone Knight", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 316
supplyStats ("Changeling", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 317
supplyStats ("Kalashtar", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 317
supplyStats ("Magewright", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 318
supplyStats ("Shifter", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 319
supplyStats ("Tarkanan Assassin", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 320
supplyStats ("Warforged Soldier", strdexconintwischa, [], ac, hp, "30 ft.", [], [], []) // Eberron - Rising from the Last War: 320

allMonsters |> Seq.find (fun m -> m.name = "Glabrezu")