module Props exposing
    ( Props, render
    , string, bool, select, counter
    , list, fieldset
    , field
    , customize
    )

{-|

@docs Props, render
@docs string, bool, select, counter
@docs list, fieldset
@docs field
@docs customize

-}

import Html.Styled as Html exposing (Html, button, div, input, legend, text)
import Html.Styled.Attributes exposing (checked, placeholder, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


type Props msg
    = String (StringProps msg)
    | Bool (BoolProps msg)
    | Select (SelectProps msg)
    | Radio (RadioProps msg)
    | Counter (CounterProps msg)
    | List (List (Props msg))
    | FieldSet String (List (Props msg))
    | Field { label : String, note : String } (Props msg)
    | Customize (Html msg)


type alias StringProps msg =
    { value : String
    , onInput : String -> msg
    , placeholder : String
    }


type alias BoolProps msg =
    { label : String
    , value : Bool
    , onClick : msg
    }


type alias SelectProps msg =
    { value : String
    , options : List String
    , onChange : String -> msg
    }


type alias RadioProps msg =
    { value : String
    , options : List String
    , onChange : String -> msg
    }


type alias CounterProps msg =
    { value : Float
    , onClickPlus : msg
    , onClickMinus : msg
    }


render : Props msg -> Html msg
render props =
    case props of
        String ps ->
            input
                [ type_ "text"
                , value ps.value
                , onInput ps.onInput
                , placeholder ps.placeholder
                ]
                []

        Bool ps ->
            Html.label []
                [ input [ type_ "checkbox", checked ps.value, onClick ps.onClick ] []
                , text ps.label
                ]

        Select ps ->
            Html.select [ onInput ps.onChange ]
                (List.map (\option -> Html.option [ value option, selected (ps.value == option) ] [ text option ])
                    ps.options
                )

        Radio ps ->
            Html.fieldset []
                (List.map
                    (\option ->
                        Html.label []
                            [ input
                                [ type_ "radio"
                                , value option
                                , checked (ps.value == option)
                                , onInput ps.onChange
                                ]
                                []
                            ]
                    )
                    ps.options
                )

        Counter ps ->
            div []
                [ button [] [ text "-" ]
                , text (String.fromFloat ps.value)
                , button [] [ text "+" ]
                ]

        List childProps ->
            div [] (List.map render childProps)

        FieldSet label childProps ->
            Html.fieldset [] <|
                legend [] [ text label ]
                    :: List.map render childProps

        Field { label, note } ps ->
            div []
                [ div [] [ Html.label [] [ text label ] ]
                , render ps
                , div [] [ text note ]
                ]

        Customize view ->
            view


string : StringProps msg -> Props msg
string =
    String


bool : BoolProps msg -> Props msg
bool =
    Bool


select : SelectProps msg -> Props msg
select =
    Select


counter : CounterProps msg -> Props msg
counter =
    Counter


list : List (Props msg) -> Props msg
list =
    List


fieldset : String -> List (Props msg) -> Props msg
fieldset =
    FieldSet


field :
    { label : String
    , props : Props msg
    , note : String
    }
    -> Props msg
field { label, note, props } =
    Field { label = label, note = note } props


customize : Html msg -> Props msg
customize =
    Customize
