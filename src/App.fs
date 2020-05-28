module App

open Compute
open Model
open Elmish
open Fable.SimpleHttp
open Thoth.Json
open Compute

let fresh =
    {
    choices = []
    constructSettings = { ConstructionSettings.creatureType = []; ConstructionSettings.sources = sources |> List.ofArray; ConstructionSettings.method = PureCR; ConstructionSettings.partySize = 4 }
    evalSettings = {
        abilities = []
        attackType = []
        dcComputer = Dynamic
        scaleEffectivenessDownByLegendaryResistance = false
        }
    creatures = NotStarted
    analysis = NotStarted
    focus = None
    showQuickview = false
    }
let init _ =
    fresh, Cmd.Empty

let update msg model =
    match msg with
    | Reset ->
        { fresh with creatures = model.creatures; choices = model.choices |> List.filter (function  Model.Wizard.TypeFilter _ | Model.Wizard.SourceFilter _ -> true | _ -> false) }, Cmd.Empty
    | LoadCreatures(Started) ->
        { model with creatures = InProgress }, Cmd.OfAsync.result (async {
            let! (statusCode, responseText) = Http.get "creatures.json"
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
    | Choose choice ->
        { model with choices = choice::model.choices }, Cmd.Empty
    | UpdateSettings(c, e) ->
        { model with constructSettings = c; evalSettings = e }, Cmd.Empty
    | SetFocus ab -> { model with focus = ab }, Cmd.Empty
    | ToggleQuickView -> { model with showQuickview = not model.showQuickview }, Cmd.Empty
