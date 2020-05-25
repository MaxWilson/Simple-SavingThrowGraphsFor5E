module Model

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
    creatures: Deferred<Result<Header array, string>>
    analysis: Deferred<Graph>
    }
type Msg =
    | LoadCreatures of AsyncOperationStatus<Result<Header array, string>>
    | Evaluate of AsyncOperationStatus<Graph>

