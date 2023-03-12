module Config exposing (string, bool, radio, select, counter)

{-|

@docs string, bool, radio, select, counter

-}

import Css exposing (color, column, displayFlex, flexDirection, hex, property)
import Html.Styled as Html exposing (Html, div, input, p, text)
import Html.Styled.Attributes exposing (checked, css, for, id, name, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Types exposing (FormState(..))
import UI.Button exposing (button, labeledButton)
import UI.Checkbox as Checkbox
import UI.Input as Input
import UI.Label exposing (basicLabel)


field : { label : String, note : String } -> Html msg -> Html msg
field { label, note } child =
    div [ css [ displayFlex, flexDirection column, property "gap" "5px" ] ]
        [ Html.label [] [ text label ]
        , child
        , p [ css [ color (hex "#999") ] ] [ text note ]
        ]


string :
    { label : String
    , value : String
    , setter : String -> msg
    , note : String
    }
    -> Html msg
string c =
    field { label = c.label, note = c.note } <|
        Input.input []
            [ input [ type_ "text", value c.value, onInput c.setter ] [] ]


bool :
    { label : String
    , id : String
    , bool : Bool
    , setter : msg
    , note : String
    }
    -> Html msg
bool c =
    field { label = "", note = c.note } <|
        Checkbox.toggleCheckbox
            { id = c.id
            , label = c.label
            , checked = c.bool
            , disabled = False
            , onClick = c.setter
            }


select :
    { label : String
    , value : option
    , options : List option
    , fromString : String -> Maybe option
    , toString : option -> String
    , setter : option -> msg
    , note : String
    }
    -> Html msg
select c =
    field { label = c.label, note = c.note } <|
        Html.select [ onInput (c.fromString >> Maybe.withDefault c.value >> c.setter) ]
            (List.map (\option -> Html.option [ value (c.toString option), selected (c.value == option) ] [ text (c.toString option) ])
                c.options
            )


radio :
    { name : String
    , value : option
    , options : List option
    , fromString : String -> Maybe option
    , toString : option -> String
    , setter : option -> msg
    }
    -> Html msg
radio c =
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
                        , onInput (c.fromString >> Maybe.withDefault c.value >> c.setter)
                        ]
                        []
                    , Html.label [ for prefixedId ] [ text (c.toString option) ]
                    ]
            )
            c.options


counter :
    { label : String
    , value : Float
    , toString : Float -> String
    , onClickPlus : msg
    , onClickMinus : msg
    , note : String
    }
    -> Html msg
counter c =
    field { label = c.label, note = c.note } <|
        labeledButton []
            [ button [ onClick c.onClickMinus ] [ text "-" ]
            , basicLabel [] [ text (c.toString c.value) ]
            , button [ onClick c.onClickPlus ] [ text "+" ]
            ]
