module Main

open Elmish
open Elmish.React
open Feliz
open Feliz.Plotly
open Fable.SimpleHttp
open Model
open View
open App
open Elmish.HMR

Program.mkProgram init update view
|> Program.withSubscription (fun model -> [
    [], (fun dispatch -> dispatch (LoadCreatures(Started)); { new System.IDisposable with member _.Dispose() = () })
    ])
|> Program.withReactSynchronous "elmish-app"
|> Program.run