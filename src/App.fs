module App

open Compute
open Model
open View
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Compute

let init _ =
    {
    constructSettings = { ConstructionSettings.creatureType = []; ConstructionSettings.sources = sources |> List.ofArray; ConstructionSettings.method = PureCR; ConstructionSettings.partySize = 4 }
    evalSettings = {
        abilities = [Str; Dex; Con; Int; Wis; Cha]
        attackType = [Save]
        dcComputer = DynamicDC
        }
    creatures = NotStarted
    analysis = NotStarted
    }, Cmd.Empty
let update msg model =
    match msg with
    | LoadCreatures(Started) ->
        { model with creatures = InProgress }, Cmd.OfAsync.result (async {
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
        })
    | LoadCreatures(Finished v) ->
        { model with creatures = Resolved(v) }, Cmd.Empty
    | Evaluate(Started) ->
        { model with analysis = InProgress }, Cmd.OfAsync.result (async {
            do! Async.Sleep 100
            try
                let evalResps = Compute.eval Compute.constructPureCR model.constructSettings model.evalSettings
                return Evaluate(Finished (Ok { settings = model.constructSettings, model.evalSettings; results = evalResps }))
            with e ->
                return Evaluate(Finished (Error ("Something went wrong" + e.ToString())))
            })
    | Evaluate(Finished v) ->
        { model with analysis = Resolved(v) }, Cmd.Empty

let view = view

