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
    dcStr: int option
    dcDex: int option
    dcCon: int option
    dcInt: int option
    dcWis: int option
    dcCha: int option
    skills: (string * int) list
    }

let parseRow a =
    let parseInt v = 
        match v with
        | RE "([0-9 ]+)" [raw] ->    
            match System.Int32.TryParse raw with
            | true, n -> n
            | _ -> failwithf "'%s' is not an int (%s)" v (a:Creature).name
        | v -> failwithf "'%s' is not an int (%s)" v a.name
    let v n =
        a.saves |> List.tryPick (fun (label, dc) -> if label = n then Some dc else None)
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
        | _ -> failwithf "Couldn't parse %s '%s'" name (a.txt.Substring(0, 100))
    let listStat name =
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
        speed = (strStat "speed") 
        dcStr = (v "Strength") 
        dcDex = (v "Dexterity") 
        dcCon = (v "Constitution") 
        dcInt = (v "Intelligence") 
        dcWis = (v "Wisdom") 
        dcCha = (v "Charisma") 
        skills = (listStat "skills")
        }
annotations <- annotations |> Map.filter (fun _ c -> c.name.Contains "Template" |> not)
let all = annotations |> Seq.map (fun (KeyValue(_, notes)) -> notes) |> Seq.sortBy (fun a -> a.name) |> Seq.map (parseRow) |> Array.ofSeq

// have to leave this part commented out because it slows down VS intellisense perf something awful, but this is how you load Newtonsoft.Json
(*
#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json
*)

System.IO.File.WriteAllText("c:\code\saves\data.json", all |> JsonConvert.SerializeObject)

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
        while str.[endIx] <> ',' && endIx + 1 < str.Length do endIx <- endIx + 1
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
        let [_;_;name;cr;size;type';tags;_;align;_;ac;hp;_;_;_;_;src] = readFields line
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
            source = src
            }
        ]
// for some reason Rising from the Last War has fewer fields
let loadExt fileName =
    let csv = System.IO.File.ReadAllLines fileName |> Seq.skip 1 |> List.ofSeq
    [for line in csv do
        printfn "%s" line
        let [_;name;cr;size;type';tags;_;align;_;ac;hp;_;_;_;_;src] = readFields line
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

save() 
load()
let mutable currentRow = allMonsters.Head
annotations <- annotations |> Map.add "Mind Flayer Lich (Illithilich)" (annotations.["Illithilich"])
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
                m.stats <- row |> parseRow |> Some
                save()
                printfn "%s OK" m.name
            with _ -> printfn "Could not parse '%s' (%s)" m.name m.source
        | None ->
            printfn "Could not find stats for '%s' (%s)" m.name m.source
        //

allMonsters |> Seq.filter (fun m -> m.stats.IsNone) |> Seq.iter (fun m -> printfn "%s needs stats" m.name)
annotations.["Mind Flayer"] |> parseRow
allMonsters |> Seq.filter (fun m -> m.name = "Mind Flayer")
save()
allMonsters.Length
allMonsters |> Seq.filter (fun m -> m.stats.IsNone) |> Seq.length
annotations.["Grazz't"]

