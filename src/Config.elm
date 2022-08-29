module Config exposing (Config(..), configToField)

import Html.Styled as Html exposing (Html, div, input, label, select, text)
import Html.Styled.Attributes exposing (checked, for, id, name, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import UI.Button exposing (button, labeledButton)
import UI.Checkbox as Checkbox
import UI.Input as Input
import UI.Label exposing (basicLabel)


type Config option msg
    = String { label : String, value : String, onInput : String -> msg }
    | Bool { id : String, label : String, bool : Bool, onClick : msg }
    | Radio
        { name : String
        , value : option
        , options : List option
        , fromString : String -> Maybe option
        , toString : option -> String
        , onChange : option -> msg
        }
    | Select
        { value : option
        , options : List option
        , fromString : String -> Maybe option
        , toString : option -> String
        , onChange : option -> msg
        }
    | Counter { value : Float, suffix : String, onClickPlus : msg, onClickMinus : msg }


configToField : Config a msg -> Html msg
configToField config =
    case config of
        String c ->
            Input.input []
                [ if c.label /= "" then
                    div [] [ text c.label ]

                  else
                    text ""
                , input [ type_ "text", value c.value, onInput c.onInput ] []
                ]

        Bool c ->
            Checkbox.checkbox
                { id = c.id
                , label = c.label
                , checked = c.bool
                , onClick = c.onClick
                }

        Radio c ->
            div [] <|
                List.map
                    (\option ->
                        let
                            prefixedId =
                                c.name ++ "_" ++ c.toString option
                        in
                        div []
                            [ input
                                [ id prefixedId
                                , type_ "radio"
                                , name c.name
                                , value (c.toString option)
                                , checked (c.value == option)
                                , onInput (c.fromString >> Maybe.withDefault c.value >> c.onChange)
                                ]
                                []
                            , label [ for prefixedId ] [ text (c.toString option) ]
                            ]
                    )
                    c.options

        Select c ->
            select [ onInput (c.fromString >> Maybe.withDefault c.value >> c.onChange) ] <|
                List.map (\option -> Html.option [ value (c.toString option), selected (c.value == option) ] [ text (c.toString option) ])
                    c.options

        Counter c ->
            labeledButton []
                [ button [ onClick c.onClickMinus ] [ text "-" ]
                , basicLabel [] [ text (String.fromFloat c.value ++ c.suffix) ]
                , button [ onClick c.onClickPlus ] [ text "+" ]
                ]
