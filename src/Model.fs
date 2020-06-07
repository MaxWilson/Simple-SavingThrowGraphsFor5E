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
    type Targeting = Single | AoE of int * float
    type Choice =
        | AnalysisType of AnalysisType
        | TargetingChoice of Targeting
        | EncounterMethod of EncounterMethod
        | Difficulty of Difficulty
        | DefenseMethod of DefenseMethod list
        | BypassMR of bool
        | ChooseDC of DC
        | PartySize of int
        | SourceFilter of string list
        | TypeFilter of string list
        | ScaleByLegendaryResist of bool
        | XanatharStyle of Compute.XanatharMethod
module Construct =
    type Analysis = | PureCR | Encounter
    type EncounterBuilder = | Xanathar | DMG | ShiningSword
    type DC = Fixed of int option | Dynamic
    type Targeting = Single | AoE of int * float
    type Settings = {
        analysis: Analysis option
        encounterBuilder: EncounterBuilder option
        difficulty: Difficulty option
        partySize: int option
        sources: string list
        types: string list
        xanatharStyle: Compute.XanatharMethod option
        }
    let empty = { analysis = None; encounterBuilder = None; difficulty = None;
                    partySize = None; sources = []; types = []; xanatharStyle = None }
    type Affordance<'t> =
        {
        heading: string
        lens: Optics.Lens<Settings, 't option>
        settings: ('t * string) list
        }
    let affordance(heading, lens, options) =
        {
        heading = heading
        lens = lens
        settings = options
        }
    let (|Analysis|_|) settings = settings.analysis
    let (|EncounterBuilder|_|) settings = settings.encounterBuilder
    let (|Difficulty|_|) settings = settings.difficulty
    let (|XanatharStyle|_|) settings = settings.xanatharStyle
    let (|PartySize|_|) settings = settings.partySize
    let analysis =
        affordance(
            "What do you want to analyze?",
            Optics.lens (fun d -> d.analysis) (fun v d -> { d with analysis = v }),
            [PureCR, "Monsters (by CR)"; Encounter, "Encounters (by PC level)"])
    let (|ConstructionSettings|_|) settings =
        let settingsFor partySize method =
            {
            ConstructionSettings.partySize = partySize
            sources = settings.sources
            creatureType = settings.types
            method = method
            } |> Some
        match settings with
        | Analysis PureCR ->
            settingsFor 1 Compute.PureCR
        | Analysis Encounter & PartySize n & EncounterBuilder(Xanathar) & Difficulty(Easy | Medium | Hard as diff) & XanatharStyle style ->
            settingsFor n (Compute.Xanathar(style, diff))
        | Analysis Encounter & PartySize n& EncounterBuilder(DMG) & Difficulty(diff) ->
            settingsFor n (Compute.DMG diff)
        | Analysis Encounter & PartySize n & EncounterBuilder(ShiningSword) & Difficulty(diff) ->
            settingsFor n (Compute.ShiningSword diff)
        | _ -> None

type Model = {
    choices: Wizard.Choice list
    construct: Construct.Settings
    constructSettings: ConstructionSettings
    evalSettings: EvaluationSettings
    creatures: Deferred<Result<Header array, string>>
    analysis: Deferred<Result<Graph, string>>
    focus: Ability option
    focusTags: string list
    showQuickview: bool
    }

type Msg =
    | LoadCreatures of AsyncOperationStatus<Result<Header array, string>>
    | Evaluate of AsyncOperationStatus<Result<Graph, string>>
    | UpdateSettings of ConstructionSettings * EvaluationSettings
    | UserChoice of (Construct.Settings -> Construct.Settings)
    | Choose of Wizard.Choice
    | Reset
    | SetFocus of Ability option
    | ToggleQuickView
    | SetFocusTags of string list
