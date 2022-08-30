module Form exposing (main)

import Browser
import Config exposing (Config(..))
import ConfigAndPreview exposing (configAndPreview)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, text, toUnstyled)
import Html.Styled.Attributes exposing (for, id, name, placeholder, rows, tabindex, type_)
import Skeleton exposing (skeleton)
import UI.Button exposing (button)
import UI.Checkbox as Checkbox exposing (checkboxWrapper)
import UI.Form as Form exposing (State(..), checkboxLabel, field, fields, form, textarea, threeFields, twoFields)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { theme : Theme, state : State }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { theme = System, state = Error }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeTheme Theme
    | UpdateConfig (Config.Msg Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        UpdateConfig configMsg ->
            ( Config.update configMsg model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        fieldsWithState options =
            [ twoFields []
                [ field
                    { type_ = "text"
                    , label = "First Name"
                    , state = options.state
                    }
                    []
                    [ Form.input { state = options.state } [ type_ "text", placeholder "First Name" ] [] ]
                , field
                    { type_ = "text"
                    , label = "Last Name"
                    , state = Default
                    }
                    []
                    [ Form.input { state = Default } [ type_ "text", placeholder "Last Name" ] [] ]
                ]
            , field
                { type_ = "checkbox"
                , label = ""
                , state = options.state
                }
                []
                [ checkboxWrapper []
                    [ Checkbox.input [ id options.id, type_ "checkbox", tabindex 0 ] []
                    , checkboxLabel { state = options.state } [ for options.id ] [ text "I agree to the Terms and Conditions" ]
                    ]
                ]
            ]
    in
    skeleton model { changeThemeMsg = ChangeTheme } <|
        List.map (\cap -> cap UpdateConfig)
            [ configAndPreview
                { title = "Form"
                , preview =
                    [ form []
                        [ field
                            { type_ = "text"
                            , label = "First Name"
                            , state = Default
                            }
                            []
                            [ Form.input { state = Default } [ type_ "text", name "first-name", placeholder "First Name" ] [] ]
                        , field
                            { type_ = "text"
                            , label = "Last Name"
                            , state = Default
                            }
                            []
                            [ Form.input { state = Default } [ type_ "text", name "last-name", placeholder "Last Name" ] [] ]
                        , field
                            { type_ = "checkbox"
                            , label = ""
                            , state = Default
                            }
                            []
                            [ checkboxWrapper []
                                [ Checkbox.input [ id "checkbox_example_1", type_ "checkbox", tabindex 0 ] []
                                , checkboxLabel { state = Default } [ for "checkbox_example_1" ] [ text "I agree to the Terms and Conditions" ]
                                ]
                            ]
                        , button [ type_ "submit" ] [ text "Submit" ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = "Field"
                , preview =
                    [ form []
                        [ field
                            { type_ = "text"
                            , label = "User Input"
                            , state = Default
                            }
                            []
                            [ Form.input { state = Default } [ type_ "text" ] [] ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = "Fields"
                , preview =
                    [ form []
                        [ fields []
                            [ field
                                { type_ = "text"
                                , label = "First Name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "First Name" ] [] ]
                            , field
                                { type_ = "text"
                                , label = "Middle name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "Middle name" ] [] ]
                            , field
                                { type_ = "text"
                                , label = "Last Name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "Last Name" ] [] ]
                            ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = ""
                , preview =
                    [ form []
                        [ threeFields []
                            [ field
                                { type_ = "text"
                                , label = "First Name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "First Name" ] [] ]
                            , field
                                { type_ = "text"
                                , label = "Middle name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "Middle name" ] [] ]
                            , field
                                { type_ = "text"
                                , label = "Last Name"
                                , state = Default
                                }
                                []
                                [ Form.input { state = Default } [ type_ "text", placeholder "Last Name" ] [] ]
                            ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = "Text Area"
                , preview =
                    [ form []
                        [ field
                            { type_ = "textarea"
                            , label = "Text"
                            , state = Default
                            }
                            []
                            [ textarea { state = Default } [] [] ]
                        , field
                            { type_ = "textarea"
                            , label = "Short Text"
                            , state = Default
                            }
                            []
                            [ textarea { state = Default } [ rows 2 ] [] ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = "Checkbox"
                , preview =
                    [ form []
                        [ field
                            { type_ = "checkbox"
                            , label = ""
                            , state = Default
                            }
                            []
                            [ checkboxWrapper []
                                [ Checkbox.input [ id "checkbox_example_2", type_ "checkbox", tabindex 0 ] []
                                , checkboxLabel { state = Default } [ for "checkbox_example_2" ] [ text "Checkbox" ]
                                ]
                            ]
                        ]
                    ]
                , configSets = []
                }
            , configAndPreview
                { title = "Form States"
                , preview = [ form [] (fieldsWithState { id = "state_example", state = model.state }) ]
                , configSets =
                    [ { label = "Form States"
                      , configs =
                            [ { label = ""
                              , config =
                                    Select
                                        { value = model.state
                                        , options = [ Default, Error, Warning, Success, Info ]
                                        , fromString = Form.stateFromString
                                        , toString = Form.stateToString
                                        , setter = \state m -> { m | state = state }
                                        }
                              , note =
                                    case model.state of
                                        Error ->
                                            "Individual fields may display an error state"

                                        Warning ->
                                            "Individual fields may display a warning state"

                                        Success ->
                                            "Individual fields may display a Success state"

                                        Info ->
                                            "Individual fields may display an informational state"

                                        Default ->
                                            ""
                              }
                            ]
                      }
                    ]
                }
            ]
