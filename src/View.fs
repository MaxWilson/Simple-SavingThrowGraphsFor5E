module View
open Compute
open Model
open Feliz
open Feliz.Plotly

let chart (lines: Map<string, float list>) =
    let colors = [(0, 100, 80); (0, 176, 246); (231, 107, 243)]
    let mkColor ix = colors.[ix % colors.Length] |> color.rgb
    let mkFill ix = colors.[ix % colors.Length] |> (fun (x,y,z) -> color.rgba(x,y,z,0.2))
    let hover key ix =
        let data = lines.[key]
        traces.scatter [
            scatter.x ([ 1 .. (data.Length) ] |> List.map float)
            scatter.y data
            scatter.marker [
                marker.color (colors.[ix % colors.Length] |> color.rgb)
                ]
            scatter.text ([ 1 .. (data.Length) ] |> List.map (fun i -> (sprintf "%s(%.1f, %.1f)" key (float i) data.[i-1])))
            scatter.hoverinfo.text
            scatter.showlegend false
            scatter.hoveron.points
            scatter.mode.markers
            ]
    let myline key ix =
        let data = lines.[key]
        traces.scatter [
            scatter.x ([ 1 .. (data.Length) ] |> List.map float)
            scatter.y data
            scatter.line [
                line.color (colors.[ix % colors.Length] |> color.rgb)
            ]
            scatter.mode.lines
            scatter.name key
            scatter.hoverinfo.skip
            ]
    let background key ix =
        let data = lines.[key]
        let xs = ([ 1 .. (data.Length) ] @ [ (data.Length) .. -1 .. 1 ]) |> List.map float
        traces.scatter [
            scatter.x xs
            scatter.y ((data |> List.map ((+) 2.))@(data |> List.rev |> List.map (fun x -> x - 2.)))
            scatter.fill.toself
            scatter.fillcolor (colors.[ix % colors.Length] |> (fun (x,y,z) -> color.rgba(x,y,z,0.2)))
            scatter.line [
                line.color color.transparent
            ]
            scatter.name key
            scatter.showlegend false
            scatter.hoverinfo.skip
        ]
    Plotly.plot [
        plot.traces [
            for name, ix in lines |> Seq.mapi(fun i (KeyValue(name,_)) -> name, i) do
                myline name ix
                background name ix
                hover name ix
        ]
        plot.layout [
            layout.paperBgcolor (color.rgb(255, 255, 255))
            layout.plotBgcolor (color.rgb(229, 229, 229))
            layout.xaxis [
                xaxis.gridcolor (color.rgb(255, 255, 255))
                xaxis.range [ 1; 10 ]
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
type Msg =
    | LoadCreatures of AsyncOperationStatus<Result<Header array, string>>
    | Evaluate of AsyncOperationStatus<Graph>
let view (model: Model) dispatch =
    match model.creatures with
    | NotStarted | InProgress -> Html.h2 "Initializing..."
    | Resolved (Error msg) ->
        Html.h2 [
            prop.style [style.color.red]
            prop.text msg
            ]
    | Resolved (Ok creatures) ->
        match model.analysis with
        | NotStarted ->
            Html.div [
                Html.text (sprintf "Loaded %d creatures" creatures.Length)
                Html.button [
                    prop.onClick (fun ev ->
                                    Evaluate Started |> dispatch
                                    )
                    prop.text "Start"
                    ]
                ]
        | InProgress ->
            Html.text "Please wait, analyzing..."
        | Resolved graph ->
            let results = graph.results
            let colorOf (attr: Compute.Ability) background =
                let r,g,b =
                    match attr with
                    | Str -> (0, 100, 80)
                    | Dex -> (0, 176, 246)
                    | Con -> (231, 107, 243)
                    | Int -> (222, 100, 80)
                    | Wis -> (200, 176, 246)
                    | Cha -> (50, 0, 243)
                if background then color.rgba(r,g,b,0.2) else color.rgb(r,g,b)
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
                                            |> List.map (fun (_, Result(_, encounters)) ->
                                                encounters
                                                |> List.exactlyOne
                                                |> (fun monsters ->
                                                    System.String.Join(" and ",
                                                        monsters
                                                        |> List.map (fun (number, m) ->
                                                            if number = 1 then m.name
                                                            else sprintf "%d %ss" number m.name)))))
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
                            scatter.hoverinfo.skip
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
                plot.traces traces
                plot.layout [
                    layout.paperBgcolor (color.rgb(255, 255, 255))
                    layout.plotBgcolor (color.rgb(229, 229, 229))
                    layout.xaxis [
                        xaxis.gridcolor (color.rgb(255, 255, 255))
                        xaxis.range [ 1; 10 ]
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
