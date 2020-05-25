module App

open Compute
open Model
open View

let init _ =
    {
    constructSettings = { ConstructionSettings.creatureType = []; ConstructionSettings.sources = sources |> List.ofArray; ConstructionSettings.method = PureCR; ConstructionSettings.partySize = 4 }
    evalSettings = {
        abilities = [Str; Dex; Con; Int; Wis; Cha]
        attackType = [Save]
        }
    creatures = NotStarted
    analysis = NotStarted
    }
let update msg model =
    match msg with
    | LoadCreatures(Started) ->
        { model with creatures = InProgress }
    | LoadCreatures(Finished v) ->
        { model with creatures = Resolved(v) }
    | Evaluate(Started) ->
        { model with analysis = InProgress }
    | Evaluate(Finished v) ->
        { model with analysis = Resolved(v) }

let view = view

