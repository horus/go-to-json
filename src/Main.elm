module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Gen
import P exposing (goStructs)
import Parser
import Random

type Model = Loading | Failed String | Success String

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ = (Loading, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg = Change String | GotText (Result String String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change structs -> (Loading, if String.isEmpty structs then Cmd.none else getJson structs)
        GotText result ->
            case result of
                Ok txt -> (Success txt, Cmd.none)
                Err err -> (Failed err, Cmd.none)

view : Model -> Browser.Document Msg
view model = { title = "import \"encoding/json\""
             , body = [ div [style "margin" "3cm", style "font-family" "monospace"] [
                        table [style "width" "1000px", style "position" "relative"] [
                          tr [] [td [] [h1 [style "text-align" "center"] [ text "Go struct to JSON" ]]],
                          tr [] [td [] [p [style "text-align" "center", style "margin-bottom" "1.5cm"] [text "A buggy, online version of json.Marshal for you!"] ] ]
                        ],
                        table [] [ thead (style "color" "white" :: displayThColor model)
                              [ th [] [text "Go struct"],
                                th [] [text "JSON"]
                              ],
                            tr []
                              [ td [][textarea [style "display" "block", style "width" "500px"
                                                , style "height" "300px", style "resize" "none"
                                                , placeholder "type or paste struct here", onInput Change] []],
                                td [] [displayResult model]
                              ]
                        ],
                        table [style "width" "1000px", style "position" "relative"]
                          [tr [] [td [] [hr [style "margin-top" "1cm", style "text-align" "center", style "width" "35%"] []]],
                           tr [] [td [] [p [style "text-align" "center", style "margin-top" "1px", style "color" "gray", style "font-size" "9pt"] [text "Made with Elm & ♥️"]]]
                          ]
                        ]
                      ]
              }

displayThColor : Model -> List (Attribute Msg)
displayThColor model =
    case model of
      Loading -> [style "background-color" "black"]
      Success bs -> if String.isEmpty bs || (String.startsWith "{" bs && String.endsWith "}" bs) then [style "background-color" "#4caf50"] else [style "background-color" "red"]
      Failed _ -> [style "background-color" "darkgray"]

displayResult : Model -> Html Msg
displayResult model =
    case model of
        Loading ->  textarea (placeholder "...loading" :: textAreaStyle)  []
        Success bs -> textarea (style "color" "blue" :: textAreaStyle) [ text bs ]
        Failed err -> textarea (style "color" "red"  :: textAreaStyle) [ text err ]

textAreaStyle : List (Attribute Msg)
textAreaStyle = [style "display" "block", style "width" "500px", style "height" "300px", style "resize" "none", readonly True ]

getJson : String -> Cmd Msg
getJson str = Random.generate GotText (Gen.fromParsed <| Parser.run goStructs str)
