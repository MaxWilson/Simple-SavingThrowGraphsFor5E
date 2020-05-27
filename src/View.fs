module View
open Compute
open Model
open Feliz
open Feliz.Plotly
open Feliz.Bulma
open Feliz.Bulma.Checkradio
open Feliz.Bulma.Switch

let header (txt: string) =
    Html.text [
        prop.className "is-large"
        prop.text txt
        ]

module Settings =
    open Compute
    open Model.Wizard
    let view (model: Model) dispatch =
        Bulma.section [
            let radioOfBase (id:string) child (isChecked: bool) msg =
                Checkradio.radio [ prop.id id; prop.name id; prop.isChecked isChecked; prop.onClick (fun _ -> dispatch (Choose msg))]
                ,Html.label [ prop.htmlFor id; child ]
            let radioOf (id:string) (txt:string) (isChecked: bool) msg =
                radioOfBase id (prop.text txt) isChecked msg
            let checkboxOf (id:string) (txt:string) (isChecked: bool) msg =
                Switch.checkbox [ Checkradio.checkradio.isMedium; prop.id id; prop.name id; prop.isChecked isChecked; prop.onClick (fun _ -> dispatch (Choose msg))]
                ,Html.label [ prop.htmlFor id; prop.text txt ]
            let group (bs: (ReactElement * ReactElement) list) =
                Bulma.field.p [
                    for radio, label in bs do
                        radio
                        label
                    ]
            let toggle choices choice =
                if choices |> List.contains choice |> not then choice::choices
                else choices |> List.filter ((<>) choice)

            let analysisChoice = model.choices |> List.tryPick((function AnalysisType(v) -> Some (v) | _ -> None))
            header "What do you want to analyze?"
            group [
                radioOf "monsters" "Monsters (by CR)" (analysisChoice = Some PureCR) (AnalysisType PureCR)
                radioOf "encounters" "Encounters (by PC level)" (analysisChoice = Some Encounter) (AnalysisType Encounter)
                ]

            if analysisChoice = Some Encounter then
                let methodChoice = model.choices |> List.tryPick((function EncounterMethod(v) -> Some (v) | _ -> None))
                header "How do you want to balance the encounters?"
                group [
                    radioOf "xanathar" "Xanathar's method" (methodChoice = Some Xanathar) (EncounterMethod Xanathar)
                    radioOf "dmg" "DMG method" (methodChoice = Some DMG) (EncounterMethod DMG)
                    radioOf "shiningSword" "ShiningSword method (modified DMG)" (methodChoice = Some ShiningSword) (EncounterMethod ShiningSword)
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
                        ]

            header "Are you targeting saving throws, ability checks, or both?"
            let defenseChoice = model.choices |> List.tryPick((function DefenseMethod(v) -> Some (v) | _ -> None)) |> Option.defaultValue []
            group [
                checkboxOf "save" "Saving throws" (defenseChoice |> List.contains Save) (DefenseMethod (toggle defenseChoice Save))
                checkboxOf "check" "Ability checks" (defenseChoice |> List.contains Check) (DefenseMethod (toggle defenseChoice Check))
                ]

            if defenseChoice |> List.contains Save then
                header "Do your attacks bypass magic resistance (e.g. Battlemaster maneuvers)?"
                let mrChoice = model.choices |> List.tryPick((function BypassMR(v) -> Some (v) | _ -> None))
                group [
                    radioOf "normalMr" "No, they are magical" (mrChoice = Some false) (BypassMR false)
                    radioOf "bypassMR" "Yes, they are nonmagical" (mrChoice = Some true) (BypassMR true)
                    ]

            if defenseChoice.Length > 0 then
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
                                prop.value (dc.ToString())
                                prop.onChange(fun str ->
                                    match System.Int32.TryParse str with
                                    | true, n -> dispatch (Choose (ChooseDC (Fixed n)))
                                    | _ -> ()
                                    )
                                ]
                            ]
                    | _ -> Html.text "Fixed DC"
                    |> prop.children
                Bulma.field.div [
                    let bs = [
                        radioOf "dynamic" "Dynamic (based on PC level)" (dcChoice = Some Dynamic) (ChooseDC Dynamic)
                        radioOfBase "fixed" fixedLabel (dcChoice |> function Some(Fixed _) -> true | _ -> false) (ChooseDC (Fixed 13))
                        ]
                    for radio, label in bs do
                        radio
                        label
                    ]


            Bulma.button.button [
                prop.onClick (fun ev ->
                                Evaluate Started |> dispatch
                                )
                prop.text "Start"
                ]
            ]


module Graph =
    let view (model: Model) (graph: Graph) dispatch =
        let results = graph.results
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
        let traces =
            [for resp in results do
                let fill = colorOf resp.ability true
                let myColor = colorOf resp.ability false
                let data = resp.results
                let bestMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.best))
                let worstMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.worst))
                let averageMarks = (data |> List.map (fun lr -> lr.pcLevel, lr.average))
                let hover =
                    let marks = bestMarks @ worstMarks
                    let xs = marks |> List.map fst
                    let ys = marks |> List.map (fun (_, Result(effectiveness, _)) -> effectiveness)

                    traces.scatter [
                        scatter.x xs
                        scatter.y ys
                        scatter.marker (marks |> List.map (fun _ -> marker.color myColor))
                        scatter.text (marks
                                        |> List.map (fun (_, Result(effectiveness, encounters)) ->
                                            let monsters =
                                                encounters
                                                |> List.exactlyOne
                                                |> (fun monsters ->
                                                    System.String.Join(" and ",
                                                        monsters
                                                        |> List.map (fun (number, m) ->
                                                            if number = 1 then m.name
                                                            else sprintf "%d %ss" number m.name)))
                                            sprintf "%A vs. %s: %.1f%% effective" resp.ability monsters effectiveness
                                                            ))
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
        Plotly.plot [
            plot.style [style.margin 20]
            plot.traces traces
            plot.layout [
                layout.paperBgcolor (color.rgb(255, 255, 255))
                layout.plotBgcolor (color.rgb(229, 229, 229))
                layout.xaxis [
                    xaxis.gridcolor (color.rgb(255, 255, 255))
                    xaxis.range [ 0.; match model.constructSettings.method with Compute.PureCR -> 30. | _ -> 20. ]
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
                ]
                layout.hovermode.closest
            ]
        ]

let view (model: Model) dispatch =
    Bulma.section [
        Bulma.container [
            Bulma.title.h2 "Shining Sword Saving Throw Analyzer"]
        match model.creatures with
        | NotStarted | InProgress -> header "Initializing..."
        | Resolved (Error msg) ->
            Html.h2 [
                prop.style [style.color.red]
                prop.text msg
                ]
        | Resolved (Ok creatures) ->
            match model.analysis with
            | NotStarted ->
                Bulma.section [
                    Settings.view model dispatch
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
                    Graph.view model graph dispatch
                    Html.button [
                        prop.onClick (fun _ -> dispatch Reset)
                        prop.text "Reset"
                        ]
                    ]
        ]