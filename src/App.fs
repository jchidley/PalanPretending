module PalanPretending

open System
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
open Elmish
open Elmish.React
module R = Fable.Helpers.React

type Model =
    { TextToWrite: string []
      TextEntered: string
      CurrentStroke: string
      WPM : int
      Accuracy : int
      LettersLearnt: int }

type Msg =
    | StrokeWritten of string
    | StrokeHint
    | Finished

let update msg model =
    match msg with
    | StrokeWritten str ->
        let textSoFar = str.Trim()
        let desiredWord = (Array.head model.TextToWrite).Trim()
        if ( desiredWord = textSoFar ) 
        then {model with 
                TextToWrite = Array.tail model.TextToWrite;
                TextEntered = "";
                CurrentStroke = Array.head (Array.tail model.TextToWrite)}
        else {model with TextEntered = textSoFar}
    | StrokeHint ->
        { model with CurrentStroke = "Stroke Hint" }
    | Finished -> {model with Accuracy = 100}

let view model dispatch =
    let header = R.div [] // .Display "none"]]
                  [ R.div [] [R.str (sprintf "WPM = %d" model.WPM)]
                    R.div [] [R.str ("Accuracy = " + (model.Accuracy.ToString()) ) ]]
    match model.TextToWrite with
    | t when Array.isEmpty t ->
        dispatch Finished
        R.div [ ]
            [ header 
              R.div [] [R.str "Finished!"]]
    | _ ->
        R.div [ ClassName "container" ]
            [ header 
              R.div [ ClassName "textToWrite"] [R.str (String.Join (" ", model.TextToWrite)) ]
              R.br []
              R.textarea [  ClassName "input"
                            Type "text"
                            Placeholder "type here"
                            AutoComplete "Off"
                            AutoFocus true
                            SpellCheck false
                            Value model.TextEntered
                            OnChange (fun ev -> !!ev.target?value |> StrokeWritten |> dispatch) ] []
              R.br []
              R.div [ ClassName "strokeToWrite" ] [ R.str model.CurrentStroke ]]

let init () =
    { TextToWrite = [| "This"; "is"; "the"; "text"; "to"; "write" |]
      CurrentStroke = "Stroke"
      TextEntered = ""
      WPM = 0
      Accuracy = 0
      LettersLearnt = 0 }

Program.mkSimple init update (lazyView2 view)
|> Program.withReact "elmish-app" // "react-element"
|> Program.run
