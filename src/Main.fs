module Main

open Elmish
open Elmish.React
open Feliz
open Feliz.Plotly
open Fable.SimpleHttp
open Model

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

open Thoth.Json
open Compute
let statsDecoder : Decoder<Stats> =
    let stat = (Decode.tuple2 Decode.int (Decode.option Decode.int))
    Decode.object(fun get -> {
        str = get.Required.At ["str"] stat
        dex = get.Required.At ["dex"] stat
        con = get.Required.At ["con"] stat
        int = get.Required.At ["int"] stat
        wis = get.Required.At ["wis"] stat
        cha = get.Required.At ["cha"] stat
        advantage = get.Optional.At ["advantage"] Decode.string |> Option.defaultValue null
        ac = get.Required.At ["ac"] Decode.int
        hp = get.Required.At ["hp"] Decode.int
        speed = get.Required.At ["speed"] Decode.string
        magicResistance = get.Required.At ["magicResistance"] Decode.bool
        dcStr = get.Optional.At ["dcStr"] Decode.int
        dcDex = get.Optional.At ["dcDex"] Decode.int
        dcCon = get.Optional.At ["dcCon"] Decode.int
        dcInt = get.Optional.At ["dcInt"] Decode.int
        dcWis = get.Optional.At ["dcWis"] Decode.int
        dcCha = get.Optional.At ["dcCha"] Decode.int
        skills = get.Optional.At ["skills"] (Decode.list (Decode.tuple2 Decode.string Decode.int)) |> Option.defaultValue []
        damageResistances = get.Optional.At ["damageResistances"] Decode.string |> Option.defaultValue null
        damageImmunities = get.Optional.At ["damageImmunities"] Decode.string |> Option.defaultValue null
        damageVuln = get.Optional.At ["damageVuln"] Decode.string |> Option.defaultValue null
        conditionImmunities = get.Optional.At ["conditionImmunities"] (Decode.list Decode.string) |> Option.defaultValue []
    })
let headerDecoder : Decoder<Header> =
    Decode.object(fun get -> {
        Header.name = get.Required.At ["name"] Decode.string
        stats = get.Required.At ["stats"] statsDecoder
        cr = get.Required.At ["cr"] Decode.float
        size = get.Required.At ["size"] Decode.string
        creatureType = get.Required.At ["creatureType"] Decode.string
        tags = get.Required.At ["tags"] (Decode.list Decode.string)
        alignment = get.Required.At ["alignment"] Decode.string
        ac = get.Required.At ["ac"] Decode.int
        hp = get.Required.At ["hp"] Decode.int
        legendary = get.Required.At ["legendary"] Decode.bool
        unique = get.Required.At ["unique"] Decode.bool
        source = get.Required.At ["source"] Decode.string
        sourcebook = get.Optional.At ["sourcebook"] Decode.string |> Option.defaultValue null
        })

open App
Program.mkSimple init update view
|> Program.withSubscription (fun model ->
    Cmd.OfAsync.result (
        async {
            let! (statusCode, responseText) = Http.get "/creatures.json"
            if statusCode = 200 then
                match Thoth.Json.Decode.fromString (Decode.array headerDecoder) (responseText) with
                | Ok creatures ->
                    Compute.initialize creatures
                    return LoadCreatures(Finished (Ok creatures))
                | Error msg ->
                    return LoadCreatures(Finished (Error msg))
            else
                return LoadCreatures(Finished(Error "Could not download creature data"))
        }
    ))
|> Program.withReactSynchronous "elmish-app"
|> Program.run