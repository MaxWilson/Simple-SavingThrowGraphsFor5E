module App

open Compute
type 't AsyncOperationStatus = Started | Finished of 't
type 't Deferred = NotStarted | InProgress | Resolved of 't
type AttackType = Save | Check | NonmagicalSave
type EvaluationSettings = {
    abilities: Ability list
    attackType: AttackType list
    }
type Graph = {
    settings: ConstructionSettings * EvaluationSettings
    results: EvaluationResponse
    }
type Model = {
    constructSettings: ConstructionSettings
    evalSettings: EvaluationSettings
    loaded: Deferred<Result<unit, string>>
    analysis: Deferred<Graph>
    }
type Msg =
    | LoadCreatures of AsyncOperationStatus<Header array>
    | Evaluate of AsyncOperationStatus<Graph>