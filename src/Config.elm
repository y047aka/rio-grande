module Config exposing (Config(..), Msg, configToField, update)

import Html.Styled as Html exposing (Html, div, input, label, select, text)
import Html.Styled.Attributes exposing (checked, for, id, name, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import UI.Button exposing (button, labeledButton)
import UI.Checkbox as Checkbox
import UI.Input as Input
import UI.Label exposing (basicLabel)


type Msg model
    = UpdateString (model -> model)
    | UpdateBool (model -> model)
    | UpdateRadio (model -> model)
    | UpdateSelect (model -> model)


update : Msg model -> model -> model
update msg =
    case msg of
        UpdateString f ->
            f

        UpdateBool f ->
            f

        UpdateRadio f ->
            f

        UpdateSelect f ->
            f


type Config model option msg
    = String
        { label : String
        , value : String
        , setter : String -> model -> model
        }
    | Bool
        { id : String
        , label : String
        , bool : Bool
        , setter : model -> model
        }
    | Radio
        { name : String
        , value : option
        , options : List option
        , fromString : String -> Maybe option
        , toString : option -> String
        , setter : option -> model -> model
        }
    | Select
        { value : option
        , options : List option
        , fromString : String -> Maybe option
        , toString : option -> String
        , setter : option -> model -> model
        }
    | Counter { value : Float, toString : Float -> String, onClickPlus : msg, onClickMinus : msg }


configToField : (Msg model -> msg) -> Config model state msg -> Html msg
configToField msg config =
    case config of
        String c ->
            Input.input []
                [ if c.label /= "" then
                    div [] [ text c.label ]

                  else
                    text ""
                , input [ type_ "text", value c.value, onInput (c.setter >> UpdateString >> msg) ] []
                ]

        Bool c ->
            Checkbox.checkbox
                { id = c.id
                , label = c.label
                , checked = c.bool
                , onClick = msg (UpdateBool c.setter)
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
                                , onInput (c.fromString >> Maybe.withDefault c.value >> c.setter >> UpdateRadio >> msg)
                                ]
                                []
                            , label [ for prefixedId ] [ text (c.toString option) ]
                            ]
                    )
                    c.options

        Select c ->
            select [ onInput (c.fromString >> Maybe.withDefault c.value >> c.setter >> UpdateSelect >> msg) ]
                (List.map (\option -> Html.option [ value (c.toString option), selected (c.value == option) ] [ text (c.toString option) ])
                    c.options
                )

        Counter c ->
            labeledButton []
                [ button [ onClick c.onClickMinus ] [ text "-" ]
                , basicLabel [] [ text (c.toString c.value) ]
                , button [ onClick c.onClickPlus ] [ text "+" ]
                ]
