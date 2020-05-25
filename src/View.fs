module View

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
let view (model: Model) dispatch =
    match model.loaded with
    | NotStarted | InProgress -> Html.h2 "Initializing..."
    | Resolved (Error msg) ->
        Html.h2 [
            prop.style [style.color.red]
            prop.text msg
            ]
    | Resolved (Ok()) ->
        Html.text "placeholder"