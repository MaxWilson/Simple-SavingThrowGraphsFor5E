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
let annotations = creatures |> Map.map (fun name txt -> { name = name; txt = txt; saves = getSaves name txt })
annotations.["Bulette"].saves <- ["Strength", 16; "Dexterity", 16]
let printRow a =
    let v n =
        match a.saves |> List.tryPick (fun (label, dc) -> if label = n then Some dc else None) with
        | Some dc -> dc.ToString()
        | None -> ""
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
            let modifier = (System.Int32.Parse raw + 1)/2 - 5
            match a.txt with
            | RE "saving_throws:[^\r](.*)" [sts] ->
                let pattern = name + "\s+([+0-9]+)"
                match sts with
                | RE pattern [save] -> sprintf "%s (%+i), %s" raw modifier save
                | _ -> sprintf "%s (%+i)," raw modifier
            | _ -> sprintf "%s (%+i)," raw modifier
        | _ -> ","
    let quoteStat name =
        let pattern = sprintf "\n%s: \"([^\"\r]+)\"" name
        match a.txt with
        | RE pattern [v] -> sprintf "\"%s\"" v
        | _ -> ""
    let str = stat "str"
    let dex = match a.txt with RE "dex: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let con = match a.txt with RE "con: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let int = match a.txt with RE "int: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let wis = match a.txt with RE "wis: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    let cha = match a.txt with RE "cha: ([0-9() +\-]+)" [stat] -> stat | _ -> "" 
    sprintf "%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s" a.name cr size type' src (stat "str") (stat "dex") (stat "con") (stat "int") (stat "wis") (stat "cha") (quoteStat "armor_class") (quoteStat "hit_points") (quoteStat "speed") (v "Strength") (v "Dexterity") (v "Constitution") (v "Intelligence") (v "Wisdom") (v "Charisma") (quoteStat "skills")
annotations.["Beholder"] |> printRow |> printfn "%s"
annotations |> Seq.map (fun (KeyValue(_, notes)) -> notes) |> Seq.sortBy (fun a -> a.name) |> Seq.map (printRow) |> fun x -> System.String.Join("\n", x) |> fun s -> System.IO.File.WriteAllText("c:\inst\exports.csv", "Name,CR,Size,Type,Src,Strength,Strength saves,Dexterity,Dexterity saves,Constitution,Constitution saves,Intelligence,Intelligence saves,Wisdom,Wisdom saves,Charisma,Charisma saves,AC,HP,Speed,Strength save DC,Dexterity save DC,Constitution save DC,Intelligence save DC,Wisdom save DC,Charisma save DC,Skills\n" + s)
