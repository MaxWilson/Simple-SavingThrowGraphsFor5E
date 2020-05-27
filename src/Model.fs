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
    type DC = Fixed of int option | Dynamic
    type Choice =
        | AnalysisType of AnalysisType
        | EncounterMethod of EncounterMethod
        | Difficulty of Difficulty
        | DefenseMethod of DefenseMethod list
        | BypassMR of bool
        | ChooseDC of DC
        | PartySize of int
type Model = {
    choices: Wizard.Choice list
    constructSettings: ConstructionSettings
    evalSettings: EvaluationSettings
    creatures: Deferred<Result<Header array, string>>
    analysis: Deferred<Result<Graph, string>>
    focus: Ability option
    }

type Msg =
    | LoadCreatures of AsyncOperationStatus<Result<Header array, string>>
    | Evaluate of AsyncOperationStatus<Result<Graph, string>>
    | UpdateSettings of ConstructionSettings * EvaluationSettings
    | Choose of Wizard.Choice
    | Reset
    | SetFocus of Ability option