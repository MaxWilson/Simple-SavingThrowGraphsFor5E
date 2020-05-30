module View
open Compute
open Model
open Feliz
open Feliz.Plotly
open Feliz.Bulma
open Feliz.Bulma.Checkradio
open Feliz.Bulma.Switch
open Feliz.Bulma.PageLoader
open Feliz.Bulma.QuickView

let header (txt: string) =
    Html.span [
        prop.className "is-large"
        prop.text txt
        ]
let radioOfBase dispatch (id:string) child (isChecked: bool) msg =
    Checkradio.radio [ prop.id id; prop.name id; prop.isChecked isChecked; prop.onCheckedChange (fun _ -> dispatch msg)]
    ,Html.label [ prop.htmlFor id; child ]
let radioOf dispatch (id:string) (txt:string) (isChecked: bool) msg =
    radioOfBase dispatch id (prop.text txt) isChecked msg
let checkboxOf dispatch (id:string) (txt:string) (isChecked: bool) msg =
    Switch.checkbox [ Checkradio.checkradio.isMedium; prop.id id; prop.name id; prop.isChecked isChecked; prop.onCheckedChange (fun _ -> dispatch msg)]
    ,Html.label [ prop.htmlFor id; prop.text txt ]
let smallCheckboxOf dispatch (id:string) (txt:string) (isChecked: bool) msg =
    Switch.checkbox [ Checkradio.checkradio.isSmall; prop.id id; prop.name id; prop.isChecked isChecked; prop.onCheckedChange (fun _ -> dispatch msg)]
    ,Html.label [ prop.htmlFor id; prop.text txt ]
let group (bs: (ReactElement * ReactElement) list) =
    Bulma.field.p [
        for radio, label in bs do
            radio
            label
        ]
let vgroup (bs: (ReactElement * ReactElement) list) =
    Html.ul [
        for radio, label in bs do
            Html.li [
                radio
                label
                ]
        ]

let toggle choices choice =
    if choices |> List.contains choice |> not then choice::choices
    else choices |> List.filter ((<>) choice)

module Settings =
    open Compute
    open Model.Wizard
    let computeSettings choices =
        let (|AnalysisChoice|_|) choices = choices |> List.tryPick((function AnalysisType(v) -> Some (v) | _ -> None))
        let (|MethodChoice|_|) choices = choices |> List.tryPick((function EncounterMethod(v) -> Some (v) | _ -> None))
        let (|MrChoice|_|) choices = choices |> List.tryPick((function BypassMR(v) -> Some (v) | _ -> None))
        let (|DifficultyChoice|_|) choices = choices |> List.tryPick((function Difficulty(v) -> Some (v) | _ -> None))
        let (|DefenseChoices|_|) choices = choices |> List.tryPick((function DefenseMethod(v) -> Some (v) | _ -> None))
        let (|DcChoice|_|) choices = choices |> List.tryPick((function ChooseDC(v) -> Some (v) | _ -> None))
        let (|PartySize|_|) choices = choices |> List.tryPick((function PartySize(v) -> Some (v) | _ -> None))
        let (|TargetingChoice|_|) = List.tryPick((function TargetingChoice(v) -> Some (v) | _ -> None))
        let (|XanatharStyleChoice|_|) = List.tryPick((function XanatharStyle(v) -> Some (v) | _ -> None))
        let (|Method|_|) = function
            | AnalysisChoice PureCR -> Some Compute.PureCR
            | AnalysisChoice Encounter & MethodChoice(method) & DifficultyChoice(diff) ->
                match method, diff with
                | Xanathar, (Easy | Medium | Hard as diff) ->
                    match choices with
                    | XanatharStyleChoice style ->
                        Some <| Compute.Xanathar(style, diff)
                    | _ -> None
                | DMG, diff -> Compute.DMG diff |> Some
                | ShiningSword, diff -> Compute.ShiningSword diff |> Some
                | _ -> None
            | _ -> None
        let (|LRChoice|_|) = List.tryPick((function ScaleByLegendaryResist(v) -> Some v | _ -> None))
        let (|LR|_|) = function LRChoice(v) -> Some v | DefenseChoices [Check] -> Some false | _ -> None
        let (|DC|_|) = function DcChoice(Fixed(Some n)) -> Compute.Fixed n |> Some | DcChoice Dynamic -> Some Compute.Dynamic | _ -> None
        let (|AttackTypes|_|) = function
            | DefenseChoices(def) & (TargetingChoice(Single) | AnalysisChoice PureCR | Method (Compute.Xanathar(Solo, _))) -> Some(def |> List.map SingleTarget)
            | DefenseChoices(def) & TargetingChoice(AoE(maxTargets, maxPct)) -> Some(def |> List.map (fun d -> Compute.AoE(d, maxTargets, maxPct)))
            | _ -> None
        let sources = choices |> List.tryPick((function SourceFilter(v) -> Some (v) | _ -> None)) |> (function None | Some [] -> (Compute.sources |> List.ofArray) | Some src -> src)
        let creatureTypes = choices |> List.tryPick((function TypeFilter(v) -> Some (v) | _ -> None)) |> Option.defaultValue []
        match choices with
        | Method (Compute.PureCR as method) & DC dc & AttackTypes(attackTypes) & LR(lrScale) ->
            let constrSettings = {
                ConstructionSettings.partySize = 1
                sources = sources
                creatureType = creatureTypes
                method = method
                }
            let evalSettings = {
                EvaluationSettings.dcComputer = dc
                abilities = [Str; Dex; Con; Int; Wis; Cha]
                attackType = attackTypes
                scaleEffectivenessDownByLegendaryResistance = lrScale
                }
            Some (constrSettings, evalSettings)
        | PartySize partySize & Method method & DC dc & AttackTypes(attackTypes) & LR(lrScale) ->
            let constrSettings = {
                ConstructionSettings.partySize = partySize
                sources = sources
                creatureType = creatureTypes
                method = method
                }
            let evalSettings = {
                EvaluationSettings.dcComputer = dc
                abilities = [Str; Dex; Con; Int; Wis; Cha]
                attackType = attackTypes
                scaleEffectivenessDownByLegendaryResistance = lrScale
                }
            Some (constrSettings, evalSettings)
        | _ -> None

    let view (model: Model) dispatch =
        React.fragment [
            let radioOfBase (id:string) child (isChecked: bool) msg = radioOfBase dispatch (id:string) child (isChecked: bool) (Choose msg)
            let radioOf (id:string) txt (isChecked: bool) msg = radioOf dispatch (id:string) txt (isChecked: bool) (Choose msg)
            let checkboxOf (id:string) txt (isChecked: bool) msg = checkboxOf dispatch (id:string) txt (isChecked: bool) (Choose msg)

            let analysisChoice = model.choices |> List.tryPick((function AnalysisType(v) -> Some (v) | _ -> None))
            header "What do you want to analyze?"
            Bulma.field.p [
                let rads = [
                    radioOf "monsters" "Monsters (by CR)" (analysisChoice = Some PureCR) (AnalysisType PureCR)
                    radioOf "encounters" "Encounters (by PC level)" (analysisChoice = Some Encounter) (AnalysisType Encounter)
                    ]
                for radio, label in rads do
                    radio
                    label
                Bulma.button.button [
                    prop.text "Set filters"
                    prop.onClick (fun _ -> dispatch ToggleQuickView)
                    ]
                ]

            if analysisChoice = Some Encounter then
                Bulma.dropdownDivider []
                let methodChoice = model.choices |> List.tryPick((function EncounterMethod(v) -> Some (v) | _ -> None))
                header "How do you want to balance the encounters?"
                group [
                    radioOf "xanathar" "Xanathar's method" (methodChoice = Some Xanathar) (EncounterMethod Xanathar)
                    radioOf "dmg" "DMG method" (methodChoice = Some DMG) (EncounterMethod DMG)
                    radioOf "shiningSword" "ShiningSword method (modified DMG for better balance with mixed monster types)" (methodChoice = Some ShiningSword) (EncounterMethod ShiningSword)
                    ]
                let xanatharChoice = model.choices |> List.tryPick((function XanatharStyle(v) -> Some (v) | _ -> None))
                if methodChoice = Some Xanathar then
                    group [
                        radioOf "groups" "Groups of monsters" (xanatharChoice = Some Group) (XanatharStyle Group)
                        radioOf "solos" "Solo monsters" (xanatharChoice = Some Solo) (XanatharStyle Solo)
                        radioOf "mixed" "Equal mix of both" (xanatharChoice = Some Mixed) (XanatharStyle Mixed)
                        ]

                if methodChoice.IsSome then
                    let difficultyChoice = model.choices |> List.tryPick((function Difficulty(v) -> Some (v) | _ -> None))
                    header "What difficulty?"
                    group [
                        radioOf "easy" "Easy" (difficultyChoice = Some Easy) (Difficulty Easy)
                        radioOf "medium" "Medium" (difficultyChoice = Some Medium) (Difficulty Medium)
                        radioOf "hard" "Hard" (difficultyChoice = Some Hard) (Difficulty Hard)
                        if methodChoice.Value <> Xanathar then
                            radioOf "deadly" "Deadly" (difficultyChoice = Some Deadly) (Difficulty Deadly)
                            radioOf "ludicrous" "Ludicrous (Deadly x2 to x6)" (difficultyChoice = Some Ludicrous) (Difficulty Ludicrous)
                        ]

                    Bulma.dropdownDivider []
                    header "How many PCs?"
                    let partySize = model.choices |> List.tryPick((function PartySize(v) -> Some (v) | _ -> None))
                    Bulma.input.number [
                        prop.maxLength 2
                        prop.style [style.maxWidth (length.em 7); style.verticalAlign.middle; style.marginLeft (length.em 2)]
                        prop.placeholder "Party size"
                        match partySize with Some dc -> prop.value (dc.ToString()) | _ -> ()
                        prop.onChange(fun str ->
                            match System.Int32.TryParse str with
                            | true, n -> dispatch (Choose (PartySize n))
                            | _ -> ()
                            )
                        ]
                // AoEs don't matter for Solo-only analysis
                if methodChoice.IsSome && not (methodChoice = Some Xanathar && (xanatharChoice.IsNone || xanatharChoice = Some Solo)) then
                    Bulma.dropdownDivider []
                    header "Can your attack hit more than one target?"
                    let targetingChoice = model.choices |> List.tryPick((function TargetingChoice(v) -> Some (v) | _ -> None))
                    let aoeLabel =
                        match targetingChoice with
                        | Some(AoE(maxTargets, maxPct)) ->
                            Html.span [
                                header "Area Effect that hits up to"
                                Bulma.input.number [
                                    prop.maxLength 4
                                    prop.style [style.maxWidth (length.em 4); style.verticalAlign.middle; style.marginLeft 5; style.marginRight 5]
                                    prop.placeholder "40%"
                                    prop.value (maxPct.ToString())
                                    prop.onChange(fun str ->
                                        match System.Double.TryParse str with
                                        | true, n -> dispatch (Choose (TargetingChoice (AoE(maxTargets, n))))
                                        | _ -> ()
                                        )
                                    ]
                                header "% of enemies, up to a maximum of"
                                Bulma.input.number [
                                    prop.maxLength 4
                                    prop.style [style.maxWidth (length.em 4); style.verticalAlign.middle; style.marginLeft 5; style.marginRight 5]
                                    prop.placeholder "max number"
                                    prop.value (maxTargets.ToString())
                                    prop.onChange(fun str ->
                                        match System.Int32.TryParse str with
                                        | true, n -> dispatch (Choose (TargetingChoice (AoE(n, maxPct))))
                                        | _ -> ()
                                        )
                                    ]
                                ]
                        | _ -> Html.text "Area Effect"
                        |> prop.children
                    Bulma.field.div [
                        let bs = [
                            radioOf "singleTarget" "Single Target" (match targetingChoice with Some(Single(_)) -> true | _ -> false) (TargetingChoice Single)
                            radioOfBase "aoe" aoeLabel (match targetingChoice with Some(AoE(_)) -> true | _ -> false) (TargetingChoice (AoE(20, 40.)))
                            ]
                        for radio, label in bs do
                            radio
                            label
                        ]

            Bulma.dropdownDivider []
            header "Are you targeting saving throws, ability checks, or both?"
            let defenseChoice = model.choices |> List.tryPick((function DefenseMethod(v) -> Some (v) | _ -> None)) |> Option.defaultValue []
            group [
                checkboxOf "save" "Saving throws" (defenseChoice |> List.contains Save) (DefenseMethod (toggle defenseChoice Save))
                checkboxOf "check" "Ability checks" (defenseChoice |> List.contains Check) (DefenseMethod (toggle defenseChoice Check))
                ]

            if defenseChoice |> List.contains Save then
                Bulma.dropdownDivider []
                header "Are your attacks affected by magic resistance?"
                let mrChoice = model.choices |> List.tryPick((function BypassMR(v) -> Some (v) | _ -> None))
                group [
                    radioOf "normalMr" "Yes, they are magical (e.g. spells)" (mrChoice = Some false) (BypassMR false)
                    radioOf "bypassMR" "No, they are nonmagical (e.g. Battlemaster maneuvers)" (mrChoice = Some true) (BypassMR true)
                    ]
                header "How would you like to count legendary resistance?"
                let lrChoice = model.choices |> List.tryPick((function ScaleByLegendaryResist(v) -> Some (v) | _ -> None))
                let effectiveLRLabel =
                    match lrChoice with
                    | Some true ->
                        Html.span [
                            header "Reduced effectiveness"
                            Html.h4 [prop.style [style.fontStyle.italic]; prop.text "Effectiveness graph will be divided by the number of saves the creature has to fail to be affected. \
                                For example, a spell that is 100% effective will be shown as 25% effective if the creature has Legendary Resistance (3/Day)."]
                            ]
                    | _ -> header "Reduced effectiveness"
                    |> prop.children
                group [
                    radioOf "normalLR" "Ignore (just show chance of a single saving throw failure)" (lrChoice = Some false) (ScaleByLegendaryResist false)
                    radioOfBase "effectiveLR" effectiveLRLabel (lrChoice = Some true) (ScaleByLegendaryResist true)
                    ]

            if defenseChoice.Length > 0 then
                Bulma.dropdownDivider []
                header "Dynamic DC or fixed?"
                let dcChoice = model.choices |> List.tryPick((function ChooseDC(v) -> Some (v) | _ -> None))

                let fixedLabel =
                    match dcChoice with
                    | Some(Fixed dc) ->
                        Html.span [
                            header "Fixed DC"
                            Bulma.input.number [
                                prop.maxLength 2
                                prop.style [style.maxWidth (length.em 4); style.verticalAlign.middle; style.marginLeft (length.em 2)]
                                prop.placeholder "DC"
                                match dc with Some dc -> prop.value (dc.ToString()) | _ -> ()
                                prop.onChange(fun str ->
                                    match System.Int32.TryParse str with
                                    | true, n -> dispatch (Choose (ChooseDC (Fixed (Some n))))
                                    | _ -> dispatch (Choose (ChooseDC (Fixed None)))
                                    )
                                ]
                            ]
                    | _ -> Html.text "Fixed DC"
                    |> prop.children
                Bulma.field.div [
                    let bs = [
                        radioOf "dynamic" "Dynamic (based on estimated PC level)" (dcChoice = Some Dynamic) (ChooseDC Dynamic)
                        radioOfBase "fixed" fixedLabel (dcChoice |> function Some(Fixed _) -> true | _ -> false) (ChooseDC (Fixed None))
                        ]
                    for radio, label in bs do
                        radio
                        label
                    ]

            let settings = (computeSettings model.choices)
            Bulma.button.button [
                prop.disabled settings.IsNone
                prop.onClick (fun ev ->
                                match settings with
                                | Some settings ->
                                    (UpdateSettings settings) |> dispatch
                                    Evaluate Started |> dispatch
                                | _ -> ()
                                )
                prop.text "Start"
                ]
            ]

module Graph =
    let colorOf (attr: Compute.Ability) background =
        let r,g,b =
            match attr with
            | Str -> (204, 0, 0) // red
            | Dex -> (51, 102, 0) // green
            | Con -> (255, 51, 153) // pink
            | Int -> (51, 51, 255) // blue
            | Wis -> (153, 0, 153) // purple
            | Cha -> (255, 255, 0) // yellow
        if background then color.rgba(r,g,b,0.1) else color.rgb(r,g,b)
    let asCardinal = function
        | 1 -> "1st"
        | 2 -> "2nd"
        | 3 -> "3rd"
        | n -> (n.ToString()) + "th"
    let addPrefix (settings: ConstructionSettings) lvl =
        match settings.method with
        | PureCR -> id // no party labels for non-encounters
        | _ ->
            fun label ->
                sprintf "%s<br>With %d %s level PCs" label settings.partySize (asCardinal lvl)
    let describeEncounter (ability: Ability) (defense:DefenseMethod) (dc: int) (encounter: Encounter) (nTargets, effectiveness) =
        let txt =
            System.String.Join(" and ",
                encounter
                |> List.map (fun (number, m) ->
                    let modifier =
                        match defense with
                        | Save | NonmagicalSave ->
                            let lr = match m.stats.legendaryResistance with
                                     | Some n -> sprintf ", LR %d/Day" n
                                     | None -> ""
                            if hasAdvantage defense ability m then
                                sprintf "(%+i, advantage%s)" (abilityOf m ability |> save) lr
                            else
                                sprintf "(%+i%s)" (abilityOf m ability |> save) lr
                        | Check ->
                            sprintf "(%+i)" (abilityOf m ability |> stat)
                    if number = 1 then sprintf "%s %s" m.name modifier
                    else sprintf "%d %ss %s" number m.name modifier))
        match nTargets with
        | None ->
            sprintf "DC %d %A %s vs. %s: %.1f%% effective" dc ability (if defense = Check then "check" else "save") txt effectiveness
        | Some nTargets ->
            sprintf "DC %d %A %s vs. %s: AoE hits %d targets and is %.1f%% effective overall" dc ability (if defense = Check then "check" else "save") txt nTargets effectiveness
    let myplot model traces =
        Plotly.plot [
            plot.style [style.margin 20]
            plot.traces traces
            plot.layout [
                layout.paperBgcolor (color.rgb(255, 255, 255))
                layout.plotBgcolor (color.rgb(229, 229, 229))
                layout.xaxis [
                    xaxis.gridcolor (color.rgb(255, 255, 255))
                    xaxis.range [ 0.; (match model.constructSettings.method with Compute.PureCR -> 30. | _ -> 20.) ]
                    xaxis.showgrid true
                    xaxis.showline false
                    xaxis.showticklabels true
                    xaxis.tickcolor (color.rgb(127, 127, 127))
                    xaxis.ticks.outside
                    xaxis.zeroline false
                ]
                layout.yaxis [
                    yaxis.gridcolor (color.rgb(255, 255, 255))
                    yaxis.showgrid true
                    yaxis.showline false
                    yaxis.showticklabels true
                    yaxis.tickcolor (color.rgb(127, 127, 127))
                    yaxis.ticks.outside
                    yaxis.zeroline false
                    yaxis.range [0.; 100.] // by default always want 0-100% effectiveness to avoid misleading
                ]
                layout.hovermode.closest
            ]
        ]
    let focused (model: Model) ability (graph: Graph) dispatch =
        let results = graph.results
        let lr = model.evalSettings.scaleEffectivenessDownByLegendaryResistance
        let traces =
            [for resp in results |> List.filter (fun r -> r.ability = ability) do
                let fill = colorOf resp.ability true
                let myColor = colorOf resp.ability false
                let data = resp.results
                let defense = match resp.attack with | SingleTarget d | AoE(d,_,_) -> d
                // in this case we're going to ignore best/worst and just plot ALL the marks
                let evaluateEncounter lvl encounter =
                    let dc = (Compute.dcOf model.evalSettings.dcComputer lvl)
                    let (nTargets, effectiveness) = Compute.calculateEffectiveness lr resp.attack resp.ability dc encounter
                    let label = describeEncounter resp.ability defense dc encounter (nTargets, effectiveness)
                    {| x = lvl; y = effectiveness; label = label |}
                let addPrefix = addPrefix model.constructSettings
                let marks = data |> List.collect(fun { pcLevel = lvl; average = Result(_, encounters) } -> encounters |> List.map (evaluateEncounter lvl))
                            |> List.groupBy(fun d -> d.x, d.y)
                            |> List.map(fun ((x,y), entries) ->
                                // If multiple monsters are on the same dot, we want to show them all
                                match entries with
                                | [singleEntry] -> singleEntry // no need to aggregate
                                | entries ->
                                    let jointLabel = System.String.Join("<br>", entries |> List.map(fun d -> d.label))
                                    {| x = x; y = y; label = jointLabel|})
                let averageMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.average))
                let hover =
                    let xs = marks |> List.map (fun d -> d.x)
                    let ys = marks |> List.map (fun d -> d.y)

                    traces.scatter [
                        scatter.x xs
                        scatter.y ys
                        scatter.marker (marks |> List.map (fun _ -> marker.color myColor))
                        scatter.text (marks
                                        |> List.map (fun d -> addPrefix (int d.x) d.label))
                        scatter.hoverinfo.text
                        scatter.showlegend false
                        scatter.hoveron.points
                        scatter.mode.markers
                        ]
                let myline =
                    traces.scatter [
                        scatter.x (averageMarks |> List.map fst)
                        scatter.y (averageMarks |> List.map (fun (_, Result(effectiveness, _)) -> effectiveness))
                        scatter.line [
                            line.color myColor
                        ]
                        scatter.mode.lines
                        scatter.name resp.name
                        scatter.text resp.name
                        scatter.hoverinfo.text
                        ]
                myline
                hover
            ]
        myplot model traces
    let overview (model: Model) (graph: Graph) dispatch =
        let results = graph.results
        let traces =
            [for resp in results do
                let fill = colorOf resp.ability true
                let myColor = colorOf resp.ability false
                let defense = match resp.attack with | SingleTarget d | AoE(d,_,_) -> d
                let data = resp.results
                let bestMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.best))
                let worstMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.worst))
                let averageMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.average))
                let hover =
                    let marks = bestMarks @ worstMarks
                    let xs = marks |> List.map fst
                    let ys = marks |> List.map (fun (_, Result(effectiveness, _)) -> effectiveness)
                    let dcOf = Compute.dcOf model.evalSettings.dcComputer

                    traces.scatter [
                        scatter.x xs
                        scatter.y ys
                        scatter.marker (marks |> List.map (fun _ -> marker.color myColor))
                        scatter.text (marks |> List.map (fun (lvl, Result(effectiveness, encounters)) -> describeEncounter resp.ability defense (dcOf lvl) (encounters |> List.exactlyOne) (None, effectiveness) |> addPrefix model.constructSettings (int lvl)))
                        scatter.hoverinfo.text
                        scatter.showlegend false
                        scatter.hoveron.points
                        scatter.mode.markers
                        ]
                let myline =
                    traces.scatter [
                        scatter.x (averageMarks |> List.map fst)
                        scatter.y (averageMarks |> List.map (fun (_, Result(effectiveness, _)) -> effectiveness))
                        scatter.line [
                            line.color myColor
                        ]
                        scatter.mode.lines
                        scatter.name resp.name
                        scatter.text resp.name
                        scatter.hoverinfo.text
                        ]
                let background =
                    let marks = bestMarks @ (List.rev worstMarks)
                    let xs = marks |> List.map fst
                    let ys = marks |> List.map (fun (_, Result(effectiveness, _)) -> effectiveness)
                    traces.scatter [
                        scatter.x xs
                        scatter.y ys
                        scatter.fill.toself
                        scatter.fillcolor fill
                        scatter.line [
                            line.color color.transparent
                        ]
                        scatter.name resp.name
                        scatter.showlegend false
                        scatter.hoverinfo.skip
                    ]
                myline
                background
                hover
            ]
        myplot model traces

let view (model: Model) dispatch =
    Html.div [
        prop.title "Shining Sword Saving Throw Analyzer"
        prop.children [
            PageLoader.pageLoader [
                pageLoader.isSuccess
                match model.creatures with
                | NotStarted | InProgress ->
                    pageLoader.isActive
                | _ -> ()
                prop.children [
                    PageLoader.title "Loading the monsters"
                    ]
                ]

            Bulma.section [
                Bulma.title.h2 "Shining Sword Saving Throw Analyzer"
                match model.creatures with
                | NotStarted | InProgress -> ()
                | Resolved (Error msg) ->
                    Html.h2 [
                        prop.style [style.color.red]
                        prop.text msg
                        ]
                | Resolved (Ok creatures) ->
                    match model.analysis with
                    | NotStarted ->
                        React.fragment [
                            Settings.view model dispatch
                            QuickView.quickview [
                                if model.showQuickview then quickview.isActive
                                prop.children [
                                    QuickView.header [
                                        Html.div "Filters"
                                        Bulma.delete [ prop.onClick (fun _ -> ToggleQuickView |> dispatch) ]
                                    ]
                                    QuickView.body [
                                        header "Sources"
                                        let sourceChoice = model.choices |> List.tryPick((function Model.Wizard.SourceFilter(v) -> Some (v) | _ -> None)) |> Option.defaultValue []
                                        vgroup [
                                            for source in Compute.sources do
                                                smallCheckboxOf dispatch ("chk" + source) source (sourceChoice.IsEmpty || sourceChoice |> List.contains source) (Model.Wizard.SourceFilter (toggle sourceChoice source) |> Choose)
                                            ]
                                        Bulma.dropdownDivider []
                                        header "Types"
                                        let creatureTypeChoices = model.choices |> List.tryPick((function Model.Wizard.TypeFilter(v) -> Some (v) | _ -> None)) |> Option.defaultValue []
                                        vgroup [
                                            for creature in Compute.creatureTypes do
                                                smallCheckboxOf dispatch ("chk" + creature) creature (creatureTypeChoices.IsEmpty || creatureTypeChoices |> List.contains creature) (Model.Wizard.TypeFilter (toggle creatureTypeChoices creature) |> Choose)
                                            ]
                                        ]
                                    QuickView.footer [
                                        Bulma.button.button [
                                            prop.text "OK"
                                            prop.onClick (fun _ -> dispatch ToggleQuickView)
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    | InProgress ->
                        Html.text "Please wait, analyzing..."
                    | Resolved (Error msg) ->
                        Html.h2 [
                            prop.style [style.color.red]
                            prop.text msg
                            ]
                    | Resolved (Ok graph) ->
                        Bulma.section [
                            match model.focus with
                            | Some ability ->
                                Graph.focused model ability graph dispatch
                            | None ->
                                Graph.overview model graph dispatch
                            Bulma.dropdownDivider[]
                            group [
                                radioOf dispatch "overview" "Overview" (model.focus = None) (SetFocus None)
                                for a in [Str; Dex; Con; Int; Wis; Cha] do
                                    let name = a.ToString()
                                    radioOf dispatch name name (model.focus = Some a) (SetFocus <| Some a)
                                ]
                            Bulma.button.button [
                                prop.onClick (fun _ -> dispatch Reset)
                                prop.text "Reset"
                                ]
                            ]
                ]
            ]
        ]