module Model

open Compute
type 't AsyncOperationStatus = Started | Finished of 't
type 't Deferred = NotStarted | InProgress | Resolved of 't
type Graph = {
    settings: ConstructionSettings * EvaluationSettings
    results: EvaluationResponse list
    }
// I don't love this method of interactively building up choices but it'll do for now
module Wizard =
    type AnalysisType = | PureCR | Encounter
    type EncounterMethod = | Xanathar | DMG | ShiningSword
    type Choice =
        | AnalysisType of AnalysisType
        | EncounterMethod of EncounterMethod
        | Difficulty of Difficulty
        | DefenseMethod of DefenseMethod list
        | BypassMR of bool
        | ChooseDC of Compute.DCComputer
type Model = {
    choices: Wizard.Choice list
    constructSettings: ConstructionSettings
    evalSettings: EvaluationSettings
    creatures: Deferred<Result<Header array, string>>
    analysis: Deferred<Result<Graph, string>>
    }

type Msg =
    | LoadCreatures of AsyncOperationStatus<Result<Header array, string>>
    | Evaluate of AsyncOperationStatus<Result<Graph, string>>
    | Choose of Wizard.Choice
    | Reset