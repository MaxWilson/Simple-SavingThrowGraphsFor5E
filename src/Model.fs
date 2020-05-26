module Model

open Compute
type 't AsyncOperationStatus = Started | Finished of 't
type 't Deferred = NotStarted | InProgress | Resolved of 't
type EvaluationSettings = {
    abilities: Ability list
    attackType: DefenseMethod list
    }
type Graph = {
    settings: ConstructionSettings * EvaluationSettings
    results: EvaluationResponse list
    }
type Model = {
    constructSettings: ConstructionSettings
    evalSettings: EvaluationSettings
    creatures: Deferred<Result<Header array, string>>
    analysis: Deferred<Result<Graph, string>>
    }

