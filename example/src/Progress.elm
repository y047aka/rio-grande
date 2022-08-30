module Progress exposing (main)

import Browser
import Config exposing (Config(..))
import ConfigAndPreview exposing (configAndPreview)
import Css exposing (int)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, toUnstyled)
import Random
import Skeleton exposing (skeleton)
import UI.Progress as Progress exposing (State(..))


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
    { theme : Theme
    , progressValue : Float
    , progressLabel : String
    , label : String
    , indicating : Bool
    , state : State
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { theme = System
      , progressValue = 0
      , progressLabel = "%"
      , label = "Uploading Files"
      , indicating = False
      , state = Default
      }
    , Random.generate NewProgress (Random.int 10 50)
    )



-- UPDATE


type Msg
    = ChangeTheme Theme
    | UpdateConfig (Config.Msg Model)
    | ProgressPlus
    | ProgressMinus
    | NewProgress Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        UpdateConfig configMsg ->
            ( Config.update configMsg model, Cmd.none )

        ProgressPlus ->
            ( model, Random.generate NewProgress (Random.int 10 15) )

        ProgressMinus ->
            ( model, Random.generate NewProgress (Random.int -15 -10) )

        NewProgress int ->
            let
                calculated =
                    model.progressValue + toFloat int

                newProgress =
                    if calculated > 100 then
                        100

                    else if calculated < 0 then
                        0

                    else
                        calculated
            in
            ( { model | progressValue = newProgress }
                |> updatelabelOnIndicating
            , Cmd.none
            )


updatelabelOnIndicating : Model -> Model
updatelabelOnIndicating model =
    { model
        | label =
            case ( model.indicating, model.progressValue == 100 ) of
                ( True, True ) ->
                    "Project Funded!"

                ( True, False ) ->
                    String.fromFloat model.progressValue ++ "% Funded"

                ( False, _ ) ->
                    model.label
    }



-- VIEW


view : Model -> Html Msg
view model =
    skeleton model { changeThemeMsg = ChangeTheme } <|
        List.map (\cap -> cap UpdateConfig)
            [ configAndPreview
                { title = "Progress"
                , preview =
                    [ Progress.progressWithProps
                        { value = model.progressValue
                        , progress = String.fromFloat model.progressValue ++ model.progressLabel
                        , label = model.label
                        , indicating = model.indicating
                        , state = model.state
                        }
                    ]
                , configSets =
                    [ { label = "Bar"
                      , configs =
                            [ { label = ""
                              , config =
                                    Counter
                                        { value = model.progressValue
                                        , toString = \value -> String.fromFloat value ++ "%"
                                        , onClickPlus = ProgressPlus
                                        , onClickMinus = ProgressMinus
                                        }
                              , note = "A progress element can contain a bar visually indicating progress"
                              }
                            ]
                      }
                    , { label = "Types"
                      , configs =
                            [ { label = ""
                              , config =
                                    Bool
                                        { id = "indicating"
                                        , label = "Indicating"
                                        , bool = model.indicating
                                        , setter =
                                            \m ->
                                                let
                                                    newIndicating =
                                                        not m.indicating
                                                in
                                                { m
                                                    | indicating = newIndicating
                                                    , label =
                                                        if newIndicating then
                                                            m.label

                                                        else
                                                            "Uploading Files"
                                                }
                                                    |> updatelabelOnIndicating
                                        }
                              , note = "An indicating progress bar visually indicates the current level of progress of a task"
                              }
                            ]
                      }
                    , { label = "States"
                      , configs =
                            [ { label = ""
                              , config =
                                    Select
                                        { value = model.state
                                        , options = [ Default, Active, Success, Warning, Error, Disabled ]
                                        , fromString = Progress.stateFromString
                                        , toString = Progress.stateToString
                                        , setter =
                                            \state m ->
                                                { m
                                                    | state = state
                                                    , label =
                                                        case state of
                                                            Success ->
                                                                "Everything worked, your file is all ready."

                                                            Warning ->
                                                                "Your file didn't meet the minimum resolution requirements."

                                                            Error ->
                                                                "There was an error."

                                                            _ ->
                                                                m.label
                                                }
                                        }
                              , note =
                                    case model.state of
                                        Active ->
                                            "A progress bar can show activity"

                                        Success ->
                                            "A progress bar can show a success state"

                                        Warning ->
                                            "A progress bar can show a warning state"

                                        Error ->
                                            "A progress bar can show an error state"

                                        Disabled ->
                                            "A progress bar can be disabled"

                                        _ ->
                                            ""
                              }
                            ]
                      }
                    , { label = "Content"
                      , configs =
                            [ { label = "Progress"
                              , config =
                                    String
                                        { label = ""
                                        , value = model.progressLabel
                                        , setter = \string m -> { m | progressLabel = string }
                                        }
                              , note = "A progress bar can contain a text value indicating current progress"
                              }
                            , { label = "Label"
                              , config =
                                    String
                                        { label = ""
                                        , value = model.label
                                        , setter = \string m -> { m | label = string }
                                        }
                              , note = "A progress element can contain a label"
                              }
                            ]
                      }
                    ]
                }
            ]
