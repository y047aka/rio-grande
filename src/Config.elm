module Config exposing
    ( Msg(..), update
    , string, bool, radio, select, counter
    )

{-|

@docs Msg, update
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


type Msg model
    = Update (model -> model)
    | CounterPlus
    | CounterMinus


update : Msg model -> model -> model
update msg =
    case msg of
        Update f ->
            f

        CounterPlus ->
            identity

        CounterMinus ->
            identity


field : { label : String, note : String } -> Html (Msg model) -> Html (Msg model)
field { label, note } child =
    div [ css [ displayFlex, flexDirection column, property "gap" "5px" ] ]
        [ Html.label [] [ text label ]
        , child
        , p [ css [ color (hex "#999") ] ] [ text note ]
        ]


string :
    { label : String
    , value : String
    , setter : String -> model -> model
    , note : String
    }
    -> Html (Msg model)
string c =
    field { label = c.label, note = c.note } <|
        Input.input []
            [ input [ type_ "text", value c.value, onInput (c.setter >> Update) ] [] ]


bool :
    { label : String
    , id : String
    , bool : Bool
    , setter : model -> model
    , note : String
    }
    -> Html (Msg model)
bool c =
    field { label = "", note = c.note } <|
        Checkbox.toggleCheckbox
            { id = c.id
            , label = c.label
            , checked = c.bool
            , disabled = False
            , onClick = Update c.setter
            }


select :
    { label : String
    , value : option
    , options : List option
    , fromString : String -> Maybe option
    , toString : option -> String
    , setter : option -> model -> model
    , note : String
    }
    -> Html (Msg model)
select c =
    field { label = c.label, note = c.note } <|
        Html.select [ onInput (c.fromString >> Maybe.withDefault c.value >> c.setter >> Update) ]
            (List.map (\option -> Html.option [ value (c.toString option), selected (c.value == option) ] [ text (c.toString option) ])
                c.options
            )


radio :
    { name : String
    , value : option
    , options : List option
    , fromString : String -> Maybe option
    , toString : option -> String
    , setter : option -> model -> model
    }
    -> Html (Msg model)
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
                        , onInput (c.fromString >> Maybe.withDefault c.value >> c.setter >> Update)
                        ]
                        []
                    , Html.label [ for prefixedId ] [ text (c.toString option) ]
                    ]
            )
            c.options


counter : { label : String, value : Float, toString : Float -> String, note : String } -> Html (Msg model)
counter c =
    field { label = c.label, note = c.note } <|
        labeledButton []
            [ button [ onClick CounterMinus ] [ text "-" ]
            , basicLabel [] [ text (c.toString c.value) ]
            , button [ onClick CounterPlus ] [ text "+" ]
            ]
