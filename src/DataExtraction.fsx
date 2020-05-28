#if NugetReferencePerfBugIsFixed // can comment back in once the perf bug is fixed, otherwise run it manually even though it's commented out

#r "nuget: Thoth.Json.Net"
open Thoth.Json.Net
let encode x = Thoth.Json.Net.Encode.Auto.toString<'t>(0, x)
let decode = match Thoth.Json.Net.Decode.Auto.fromString<'t> with Ok v -> v | Error msg -> failwithf "Error deserializing! %s" msg
*)
#else
// we don't really use these implementations
let encode x : string = failwith "Not implemented"
let decode<'t> (input:string) : 't = failwith "Not implemented"
#endif
open System
open System.IO
open System.Text.RegularExpressions
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
    mutable sourcebook: string
    }


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
System.IO.File.WriteAllText(@"c:\code\saves\markdown.json", creatures |> encode)


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
        magicResistance = match a.txt with RE "(Magic Resistance|Magic Immunity)" [_] -> true | _ -> false
        advantage = match a.txt with RE "advantage on (\s+) saving throws" [attr] -> "attr" | RE "advantage on saving throws against (being )?(\w+)" [_;effect] -> effect | _ -> null
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
        legendaryResistance = match a.txt with RE "Legendary Resistance \(([0-9])/Day\)" [n] -> Some (System.Int32.Parse n) | _ -> None
        }
// For entries like "Demonic Boon Template" we don't care if they can't be parsed, they won't be used anyway
let all = annotations |> Seq.map (fun (KeyValue(_, notes)) -> notes) |> Seq.sortBy (fun a -> a.name) |> Seq.choose (fun x -> try parseRow x |> Some with _ when x.name.Contains "Template" -> None) |> Array.ofSeq

System.IO.File.WriteAllText(@"c:\code\saves\annotations.json", all |> encode)


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
            sourcebook = null
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
            sourcebook = null
            }
        ]

// it turns out to be convenient to define a mutable variable for the data we're working on
let mutable allMonsters =
    loadKobold "c:\code\saves\kfc official - Monsters.csv"
    @ loadExt "c:\code\saves\kfc eberron last war - Monsters.csv"

let save () =
    System.IO.File.WriteAllText(@"c:\code\saves\data.json", encode allMonsters)
let load () =
    allMonsters <- System.IO.File.ReadAllText(sprintf @"c:\code\saves\data.json") |> decode<KoboldRow list>

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
            printfn "supplyStats (\"%s\", achpstrdexconintwischa, [], \"30 ft.\", [], [], []) // %s" m.name m.source
        //

// remove monsters that aren't real monsters deserving of a separate entry
save()

type SavingThrow = Str of int | Dex of int | Con of int | Int of int | Wis of int | Cha of int | MR | Advantage of string | Legendary of int
type Condition = Blinded | Charmed | Exhaustion | Frightened | Poisoned | Prone | Restrained | Deafened | Paralyzed | Unconscious | Petrified | Stunned | Grappled | Incapacitated
type Resist = Vuln of string | Resist of string | Immune of string | Conditions of Condition list
type Prof = Arcana | History | Insight | Religion | Performance | Perception | Persuasion | Stealth | Acrobatics | Deception | Athletics | Intimidation | Medicine | Nature | Sleight | Survival
let supplyStats args =
    let (
                name: string,
                ac: int,
                hp: int,
                str: int,
                dex: int,
                con: int,
                int: int,
                wis: int,
                cha: int,
                saves: SavingThrow list,
                speed: string,
                skills: (Prof * int) list,
                resists: Resist list,
                dc: SavingThrow list
            ) = args
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
            advantage = saves |> List.tryPick(function Advantage attr -> Some attr | _ -> None) |> function Some v -> v | None -> null
            speed = speed
            skills = skills |> List.map (function (Sleight, bonus) -> "Sleight of Hand", bonus | (skill, bonus) -> skill.ToString(), bonus)
            damageVuln = resists |> List.tryPick(function Vuln x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            damageResistances = resists |> List.tryPick(function Resist x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            damageImmunities = resists |> List.tryPick(function Immune x -> Some x | _ -> None) |> (function Some x -> x | _ -> null)
            conditionImmunities = resists |> List.tryPick(function Conditions x -> Some x | _ -> None) |> (function Some x -> x |> List.map (fun x -> (sprintf "%A" x).ToLowerInvariant()) | _ -> [])
            dcStr = dc |> List.tryPick(function Str x -> Some x | _ -> None)
            dcDex = dc |> List.tryPick(function Dex x -> Some x | _ -> None)
            dcCon = dc |> List.tryPick(function Con x -> Some x | _ -> None)
            dcInt = dc |> List.tryPick(function Int x -> Some x | _ -> None)
            dcWis = dc |> List.tryPick(function Wis x -> Some x | _ -> None)
            dcCha = dc |> List.tryPick(function Cha x -> Some x | _ -> None)
            legendaryResistance = saves |> List.tryPick(function (Legendary x) -> Some x | _ -> None)
            }

supplyStats ("Giant Ice Toad", 14, 52, 16, 13, 14, 8, 10, 6, [], "30 ft.", [], [], []) // Tales from the Yawning Portal: 235
supplyStats ("Giant Skeleton", 17, 115, 21, 10, 20, 4, 6, 6, [], "30 ft.", [], [Vuln "bludgeoning"; Immune "poison"; Conditions [Exhaustion; Poisoned]], []) // Tales from the Yawning Portal: 236
supplyStats ("Thayan Apprentice", 12, 27, 10, 14, 12, 15, 13, 11, [], "30 ft.", [Arcana, 4], [], []) // Tales from the Yawning Portal: 245
supplyStats ("Acererak", 21, 285, 13, 16, 20, 27, 21, 20, [Con 12; Int 15; Wis 12; Legendary 3], "30 ft.", [Arcana, 22; History, 22; Insight, 12; Perception, 12; Religion, 15], [], [Con 23; Wis 20]) // Tomb of Annihilation: 209
supplyStats ("Tabaxi Ministrel", 12, 22, 10, 15, 11, 14, 12, 16, [], "30 ft., climb 20 ft.", [Perception, 3; Performance, 7; Persuasion, 5; Stealth, 4], [Resist "cold, lightning"; Immune "necrotic, poison, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Blinded; Charmed; Deafened; Exhaustion; Frightened; Paralyzed; Petrified; Poisoned; Stunned]], []) // Tomb of Annihilation: 233
supplyStats ("Belashyrra", 19, 304, 24, 21, 20, 25, 22, 23, [Int 14; Wis 13; Cha 13; MR], "40 ft., fly 40 ft. (hover)", [Arcana, 14; Perception, 13], [Resist "poison, psychic"; Conditions [Blinded; Charmed; Exhaustion; Frightened; Poisoned; Prone]], [Wis 22; Int 22; Con 22]) // Eberron - Rising from the Last War: 286
supplyStats ("Dyrrn", 21, 325, 26, 21, 22, 26, 23, 24, [Int 15; Wis 13; Cha 14; MR], "40 ft., fly 40 ft. (hover).", [Arcana, 15; History, 15; Insight, 13; Perception, 13], [Resist "poison, psychic"; Conditions [Blinded; Charmed; Exhaustion; Frightened; Poisoned; Prone]], [Wis 22; Con 23; Int 23]) // Eberron - Rising from the Last War: 288
supplyStats ("Clawfoot", 13, 19, 12, 16, 14, 4, 12, 6, [], "40 ft.", [Perception, 3; Stealth, 5], [], []) // Eberron - Rising from the Last War: 289
supplyStats ("Fastieth", 14, 9, 12, 18, 10, 4, 11, 4, [], "50 ft.", [], [], []) // Eberron - Rising from the Last War: 289
supplyStats ("Dolgaunt", 16, 33, 14, 18, 12, 13, 14, 11, [], "40 ft.", [Acrobatics, 6; Perception, 4; Stealth, 6], [Conditions [Blinded]], [Con 11]) // Eberron - Rising from the Last War: 290
supplyStats ("Dolgrim", 15, 13, 15, 14, 12, 8, 10, 8, [], "30 ft.", [], [], []) // Eberron - Rising from the Last War: 291
supplyStats ("Dusk Hag", 17, 82, 11, 14, 12, 17, 16, 18, [Int 6; Wis 6; MR], "30 ft.", [Deception, 7; Insight, 6; Perception, 6], [Conditions[Blinded; Charmed; Frightened]], [Wis 15]) // Eberron - Rising from the Last War: 292
supplyStats ("Expeditious Messenger", 13, 7, 6, 16, 13, 8, 12, 7, [], "25 ft., fly 60 ft.", [Acrobatics, 5; Stealth, 5], [Immune "poison"; Conditions [Exhaustion; Poisoned]], []) // Eberron - Rising from the Last War: 293
supplyStats ("Iron Defender", 17, 30, 16, 14, 16, 8, 11, 7, [], "40 ft.", [Perception, 3; Stealth, 4], [Immune "poison"; Conditions [Exhaustion; Poisoned]], [Str 13]) // Eberron - Rising from the Last War: 293
supplyStats ("Inspired", 12, 40, 11, 14, 10, 16, 10, 16, [Int 5; Wis 2; Advantage "Wisdom"], "30 ft.", [Deception, 7; Insight, 2; Persuasion, 7], [Resist "psychic"; Conditions [Charmed; Frightened]], [Wis 13]) // Eberron - Rising from the Last War: 294
supplyStats ("Karrnathi Undead Soldier", 17, 52, 16, 14, 16, 12, 13, 5, [], "30 ft.", [Athletics, 5; Perception, 5], [Resist "cold, poison"; Conditions [Charmed; Frightened; Poisoned]], []) // Eberron - Rising from the Last War: 295
supplyStats ("Lady Illmarrow", 19, 199, 16, 16, 20, 27, 21, 24, [Con 12; Int 15; Wis 12; MR; Legendary 3], "30 ft., fly 40 ft.", [Arcana, 15; History, 15; Insight, 12; Perception, 12], [Resist "cold, lightning"; Immune "necrotic, poison, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Blinded; Charmed; Deafened; Exhaustion; Frightened; Paralyzed; Petrified; Poisoned; Stunned]], [Con 20; Wis 20]) // Eberron - Rising from the Last War: 297
supplyStats ("Living Burning Hands", 15, 15, 10, 12, 16, 3, 6, 6, [MR], "25 ft., fly 25 ft. (hover)", [], [Resist "bludgeoning, piercing, and slashing from nonmagical attacks"; Immune "fire"; Conditions [Blinded; Charmed; Deafened; Exhaustion; Frightened; Grappled; Poisoned; Prone]], [Dex 13]) // Eberron - Rising from the Last War: 298
supplyStats ("Living Lightning Bolt", 15, 57, 10, 15, 18, 3, 10, 6, [MR], "25 ft., fly 25 ft. (hover)", [], [Resist "bludgeoning, piercing, and slashing from nonmagical attacks"; Immune "fire"; Conditions [Blinded; Charmed; Deafened; Exhaustion; Frightened; Grappled; Poisoned; Prone]], [Dex 15]) // Eberron - Rising from the Last War: 299
supplyStats ("Living Cloudkill", 15, 73, 10, 15, 14, 3, 11, 6, [], "25 ft., fly 25 ft. (hover)", [], [Resist "bludgeoning, piercing, and slashing from nonmagical attacks"; Immune "fire"; Conditions [Blinded; Charmed; Deafened; Exhaustion; Frightened; Grappled; Poisoned; Prone]], [Con 16]) // Eberron - Rising from the Last War: 299
supplyStats ("The Lord of Blades", 19, 195, 20, 15, 18, 19, 17, 18, [Str 11; Con 10; Int 10; Wis 9; Advantage "poison"], "40 ft.", [Arcana, 10; Athletics, 11; History, 10; Perception, 9], [Resist "necrotic, poison"; Conditions [Charmed; Exhaustion; Frightened]], [Str 19]) // Eberron - Rising from the Last War: 300
supplyStats ("Mordakhesh", 18, 170, 20, 16, 18, 15, 17, 20, [Str 10; Con 9; Wis 8; Cha 10; MR], "40 ft.", [Athletics, 10; Insight, 8; Perception, 8; Persuasion, 10], [Vuln "piercing from magic weapons wielded by good creatures"; Resist "bludgeoning, piercing, and slashing from nonmagical attacks not made with silvered weapons"], [Wis 18; Con 18]) // Eberron - Rising from the Last War: 301
supplyStats ("Rak Tulkhesh", 23, 478, 29, 19, 27, 21, 22, 26, [Str 17; Con 16; Wis 14; Cha 16; MR; Legendary 3], "40 ft., climb 40 ft., fly 80 ft.", [Athletics, 17; Intimidation, 16; Perception, 14], [Resist "cold, fire, lightning"; Immune "poison, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Charmed; Exhaustion; Frightened; Paralyzed; Poisoned; Stunned]], [Wis 24; Con 24]) // Eberron - Rising from the Last War: 303
supplyStats ("Sul Khatesh", 22, 475, 18, 21, 19, 30, 22, 25, [Con 12; Int 18; Wis 14; Cha 15; MR; Legendary 3; Advantage "concentration"], "40 ft., fly 40 ft. (hover)", [Arcana, 18; History, 18; Insight, 14; Religion, 18], [Resist "cold, fire, lightning"; Immune "poison, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Charmed; Exhaustion; Frightened; Paralyzed; Petrified; Poisoned]], [Wis 26; Dex 26; Con 26]) // Eberron - Rising from the Last War: 304
supplyStats ("Hashalaq Quori", 17, 99, 12, 14, 13, 18, 16, 18, [Wis 7; Cha 8], "40 ft.", [Arcana, 12; History, 12; Insight, 12; Persuasion, 8], [Resist "psychic"; Conditions [Charmed; Frightened]], [Wis 16; Int 16; Cha 16]) // Eberron - Rising from the Last War: 305
supplyStats ("Kalaraq Quori", 18, 161, 12, 21, 18, 23, 24, 25, [Int 12; Wis 13; Cha 13; MR], "30 ft., fly 60 ft. (hover)", [Deception, 13; Perception, 13; Persuasion, 13], [Resist "cold, necrotic, poison, psychic, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Blinded; Charmed; Exhaustion; Frightened; Grappled; Paralyzed; Petrified; Prone; Restrained]], [Int 21; Wis 21; Cha 21]) // Eberron - Rising from the Last War: 306
supplyStats ("Tsucora Quori", 16, 68, 17, 14, 18, 14, 14, 16, [Wis 5; Cha 6], "40 ft.", [Insight, 5; Perception, 6], [Resist "psychic"; Conditions [Charmed; Frightened]], [Wis 14; Cha 14]) // Eberron - Rising from the Last War: 307
supplyStats ("Radiant Idol", 18, 142, 23, 18, 19, 17, 20, 21, [Wis 9; Cha 9; MR], "40 ft.", [Deception, 9; Insight, 9; Perception, 9; Persuasion, 9], [Resist "radiant, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Charmed; Exhaustion; Frightened]], [Wis 17; Con 17]) // Eberron - Rising from the Last War: 308
supplyStats ("Zakya Rakshasa", 18, 59, 18, 14, 18, 12, 13, 11, [MR], "30 ft.", [Athletics, 7; Perception, 4], [Vuln "piercing from magic weapons wielded by good creatures"; Resist "bludgeoning, piercing, and slashing from nonmagical attacks"], [Str 15; Wis 15]) // Eberron - Rising from the Last War: 309
supplyStats ("Undying Councilor", 17, 104, 16, 10, 14, 17, 21, 16, [Con 6; Int 7; Wis 9; MR], "30 ft.", [Arcana, 7; History, 11; Insight, 9; Perception, 9; Religion, 7], [Vuln "necrotic"; Immune "poison, radiant"; Conditions [Charmed; Exhaustion; Frightened; Paralyzed; Poisoned]], [Dex 17; Con 17]) // Eberron - Rising from the Last War: 311
supplyStats ("Undying Soldier", 17, 26, 16, 12, 14, 11, 13, 14, [], "30 ft.", [Athletics, 5; History, 4; Perception, 3; Religion, 4], [Vuln "necrotic"; Resist "radiant, bludgeoning, piercing, and slashing from nonmagical attacks not made with silvered weapons"; Immune "poison"; Conditions [Exhaustion; Poisoned]], []) // Eberron - Rising from the Last War: 311
supplyStats ("Valenar Hawk", 14, 10, 8, 18, 10, 9, 16, 11, [], "10 ft., fly 60 ft.", [Perception, 5], [], []) // Eberron - Rising from the Last War: 312
supplyStats ("Valenar Hound", 14, 19, 17, 15, 14, 10, 15, 11, [], "40 ft.", [Perception, 4], [], []) // Eberron - Rising from the Last War: 312
supplyStats ("Valenar Steed", 13, 22, 14, 16, 14, 10, 15, 11, [], "60 ft.", [Perception, 4], [], []) // Eberron - Rising from the Last War: 313
supplyStats ("Warforged Colossus", 23, 410, 30, 11, 30, 3, 11, 8, [Int 4; Wis 8; Cha 7; MR], "60 ft.", [], [Immune "necrotic, poison, bludgeoning, piercing, and slashing from nonmagical attacks"; Conditions [Charmed; Exhaustion; Frightened; Incapacitated; Paralyzed; Petrified; Poisoned; Stunned]], [Wis 26; Dex 26]) // Eberron - Rising from the Last War: 314
supplyStats ("Warforged Titan", 20, 125, 23, 8, 22, 3, 11, 1, [], "40 ft.", [], [Immune "poison, psychic"; Conditions [Charmed; Exhaustion; Frightened; Paralyzed; Petrified; Poisoned]], [Str 17; Dex 17]) // Eberron - Rising from the Last War: 315
supplyStats ("Bone Knight", 20, 84, 18, 13, 14, 12, 14, 16, [Wis 5; Cha 6], "30 ft.", [Athletics, 7; Deception, 6; Intimidation, 6], [Resist "necrotic, poison"], [Wis 14]) // Eberron - Rising from the Last War: 316
supplyStats ("Changeling", 13, 22, 8, 15, 12, 14, 10, 16, [], "30 ft.", [Acrobatics, 4; Deception, 5; Insight, 2; Perception, 2; Persuasion, 5], [], [Wis 13]) // Eberron - Rising from the Last War: 317
supplyStats ("Kalashtar", 12, 16, 12, 14, 12, 13, 15, 15, [Advantage "Wisdom"], "30 ft.", [Acrobatics, 4; Insight, 4; Persuasion, 6], [Resist "psychic"], [Wis 12]) // Eberron - Rising from the Last War: 317
supplyStats ("Magewright", 11, 9, 11, 13, 10, 14, 14, 12, [], "30 ft.", [Arcana, 4; Performance, 4; Medicine, 4; Insight, 4; Persuasion, 5; Deception, 3; Religion, 4; History, 4], [], [Cha 12]) // Eberron - Rising from the Last War: 318
supplyStats ("Shifter", 14, 19, 12, 16, 14, 11, 15, 10, [], "30 ft.", [Acrobatics, 5; Insight, 4; Nature, 2; Perception, 4], [], []) // Eberron - Rising from the Last War: 319
supplyStats ("Tarkanan Assassin", 15, 45, 12, 16, 14, 10, 14, 11, [], "30 ft.", [Athletics, 3; Deception, 2; Perception, 4; Sleight, 5; Stealth, 5], [], [Con 12]) // Eberron - Rising from the Last War: 320
supplyStats ("Warforged Soldier", 16, 30, 16, 12, 16, 10, 14, 11, [Advantage "poison"], "30 ft.", [Athletics, 5; Perception, 4; Survival, 4], [], []) // Eberron - Rising from the Last War: 320

save()

for m in allMonsters |> Seq.filter (fun m -> m.sourcebook = null) do
    let sources = ["Out of the Abyss"; "Princes of the Apocalypse";
        "Hoard of the Dragon Queen"; "Rise of Tiamat"; "Monster Manual";
        "Princes of the Apocalypse Online Supplement v1"; "Basic Rules v1";
        "HotDQ supplement"; "Player's Handbook"; "Mordenkainen's Tome of Foes";
        "Storm King's Thunder"; "Curse of Strahd"; "Tales from the Yawning Portal";
        "Tomb of Annihilation"; "The Tortle Package"; "Volo's Guide to Monsters";
        "Eberron - Rising from the Last War"]
    match sources |> List.tryFind(fun candidate -> m.source.StartsWith candidate) with
    | Some src -> m.sourcebook <- src
    | None -> printfn "Not categorized: %s from %s" m.name m.source

save()

// remove the stupid Tortle stuff
allMonsters <- allMonsters |> List.filter (fun m -> m.stats.IsSome)

// #r "nuget: Thoth.Json.Net"
// open Thoth.Json.Net
// put the data in a format that we can easily parse at runtime (Thoth, not Newtonsoft)
System.IO.File.WriteAllText(@"c:\code\saves\public\creatures.json", allMonsters |> encode)

