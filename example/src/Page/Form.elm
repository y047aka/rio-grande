module Page.Form exposing (Model, Msg, init, update, view)

import Config
import ConfigAndPreview exposing (configAndPreview)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (placeholder, rows, type_)
import Shared
import Types exposing (FormState(..), formStateFromString, formStateToString)
import UI.Button exposing (button)
import UI.Checkbox as Checkbox
import UI.Form as Form exposing (field, fields, form, textarea, threeFields, twoFields)



-- INIT


type alias Model =
    { checked : Bool
    , state : FormState
    }


init : ( Model, Cmd Msg )
init =
    ( { checked = False
      , state = Error
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ToggleChecked
    | UpdateConfig (Config.Msg Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleChecked ->
            ( { model | checked = not model.checked }, Cmd.none )

        UpdateConfig configMsg ->
            ( Config.update configMsg model, Cmd.none )



-- VIEW


view : Shared.Model -> Model -> List (Html Msg)
view shared model =
    [ configAndPreview
        { title = "Form"
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
    , configAndPreview
        { title = "Field"
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
    , configAndPreview
        { title = "Fields"
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
    , configAndPreview
        { title = ""
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
    , configAndPreview
        { title = "Text Area"
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
    , configAndPreview
        { title = "Checkbox"
        , toMsg = UpdateConfig
        , theme = shared.theme
        , inverted = False
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
