module App

open Compute
open Model
open View
open Elmish
open Fable.SimpleHttp
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


let init _ =
    {
    constructSettings = { ConstructionSettings.creatureType = []; ConstructionSettings.sources = sources |> List.ofArray; ConstructionSettings.method = PureCR; ConstructionSettings.partySize = 4 }
    evalSettings = {
        abilities = [Str; Dex; Con; Int; Wis; Cha]
        attackType = [Save]
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
            let linesOf (settings: EvaluationSettings) =
                [
                    for ability in settings.abilities do
                        for attackType in settings.attackType do
                            for ability in [Str; Dex; Con; Int; Wis; Cha] do
                                Compute.Evaluation(
                                    (match attackType with
                                        | Save -> ability.ToString()
                                        | Check -> sprintf "%A check" ability
                                        | NonmagicalSave -> sprintf "%A (bypass MR)" ability),
                                    // for now, no AoEs are supported
                                    SingleTarget(attackType),
                                    ability)

                ]
            do! Async.Sleep 100
            let evalResps = Compute.eval Compute.constructPureCR model.constructSettings (linesOf model.evalSettings)
            return Evaluate(Finished { settings = model.constructSettings, model.evalSettings; results = evalResps })
            })
    | Evaluate(Finished v) ->
        { model with analysis = Resolved(v) }, Cmd.Empty

let view = view

