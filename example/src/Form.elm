module Form exposing (main)

import Browser
import Config
import ConfigAndPreview exposing (configAndPreview)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, text, toUnstyled)
import Html.Styled.Attributes exposing (placeholder, rows, type_)
import Skeleton exposing (skeleton)
import Types exposing (FormState(..), formStateFromString, formStateToString)
import UI.Button exposing (button)
import UI.Checkbox as Checkbox
import UI.Form as Form exposing (field, fields, form, textarea, threeFields, twoFields)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    { theme : Theme
    , checked : Bool
    , state : FormState
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { theme = System
      , checked = False
      , state = Error
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeTheme Theme
    | ToggleChecked
    | UpdateConfig (Config.Msg Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        ToggleChecked ->
            ( { model | checked = not model.checked }, Cmd.none )

        UpdateConfig configMsg ->
            ( Config.update configMsg model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    skeleton { theme = model.theme, changeThemeMsg = ChangeTheme }
        [ configAndPreview UpdateConfig { theme = model.theme } <|
            { title = "Form"
            , preview =
                [ form []
                    [ twoFields []
                        [ field
                            { type_ = "text"
                            , label = "First Name"
                            , state = model.state
                            }
                            []
                            [ Form.input { state = model.state } [ type_ "text", placeholder "First Name" ] [] ]
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
                        , state = model.state
                        }
                        []
                        [ Checkbox.checkboxWithProps
                            { id = "state_example"
                            , label = "I agree to the Terms and Conditions"
                            , checked = model.checked
                            , disabled = False
                            , state = model.state
                            , onClick = ToggleChecked
                            }
                        ]
                    , button [ type_ "submit" ] [ text "Submit" ]
                    ]
                ]
            , configSections =
                [ { label = "Form States"
                  , configs =
                        [ { label = ""
                          , config =
                                Config.select
                                    { value = model.state
                                    , options = [ Default, Error, Warning, Success, Info ]
                                    , fromString = formStateFromString
                                    , toString = formStateToString
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
        , configAndPreview UpdateConfig { theme = model.theme } <|
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
            , configSections = []
            }
        , configAndPreview UpdateConfig { theme = model.theme } <|
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
            , configSections = []
            }
        , configAndPreview UpdateConfig { theme = model.theme } <|
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
            , configSections = []
            }
        , configAndPreview UpdateConfig { theme = model.theme } <|
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
            , configSections = []
            }
        , configAndPreview UpdateConfig { theme = model.theme } <|
            { title = "Checkbox"
            , preview =
                [ form []
                    [ field
                        { type_ = "checkbox"
                        , label = ""
                        , state = Default
                        }
                        []
                        [ Checkbox.checkbox
                            { id = "checkbox_example_2"
                            , label = "Checkbox"
                            , disabled = False
                            , checked = model.checked
                            , onClick = ToggleChecked
                            }
                        ]
                    ]
                ]
            , configSections = []
            }
        ]
