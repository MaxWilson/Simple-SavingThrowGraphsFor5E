module App

open Elmish
open Elmish.React
open Feliz

open Feliz.Plotly

let chart (lines: Map<string, float list>) =
    let colors = [(0, 100, 80); (0, 176, 246); (231, 107, 243)]
    let mkColor ix = colors.[ix % colors.Length] |> color.rgb
    let mkFill ix = colors.[ix % colors.Length] |> (fun (x,y,z) -> color.rgba(x,y,z,0.2))
    let myline key ix =
        let data = lines.[key]
        traces.scatter [
            scatter.x [ 1 .. (data.Length) ]
            scatter.y data
            scatter.line [
                line.color (colors.[ix % colors.Length] |> color.rgb)
            ]
            scatter.mode.lines
            scatter.name key
            ]
    let background key ix =
        let data = lines.[key]
        let xs: int list = ([ 1 .. (data.Length) ] @ [ (data.Length) .. -1 .. 1 ])
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
        ]
    Plotly.plot [
        plot.traces [
            for name, ix in lines |> Seq.mapi(fun i (KeyValue(name,_)) -> name, i) do
                background name ix
                myline name ix
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
        ]
    ]

type State =
    { Count: int; lines: Map<string, float list> }

type Msg =
    | Increment
    | Decrement

let lines =
    [
    "Fair", [1..10] |> List.map float
    "Premium", [ 5.; 2.5; 5.; 7.5; 5.; 2.5; 7.5; 4.5; 5.5; 5. ]
    "Ideal", [ 10; 8; 6; 4; 2; 0; 2; 4; 2; 0 ] |> List.map float
    ] |> Map.ofList

let r = System.Random()
let setLength (s: State) =
    let l = s.Count
    let adjust _ (line: float list) =
        if line.Length = l then line
        elif line.Length > l then List.take l line
        else line @ [r.NextDouble() * 10.]
    { s with lines = s.lines |> Map.map adjust }


let init() =
    { Count = 0; lines = lines }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1 } |> setLength

    | Decrement ->
        { state with Count = state.Count - 1 } |> setLength


let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    Html.h1 state.Count
    chart state.lines
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run