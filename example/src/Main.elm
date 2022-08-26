module Main exposing (main)

import Browser
import ConfigAndPreview exposing (configAndPreview)
import Css exposing (int)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, input, option, select, text, toUnstyled)
import Html.Styled.Attributes exposing (selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random
import Skeleton exposing (skeleton)
import UI.Button exposing (button, labeledButton)
import UI.Checkbox exposing (checkbox)
import UI.Input as Input
import UI.Label exposing (basicLabel)
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
    | ProgressPlus
    | ProgressMinus
    | NewProgress Int
    | EditProgressLabel String
    | EditLabel String
    | ToggleIndicating
    | ChangeState State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )

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

        EditProgressLabel string ->
            ( { model | progressLabel = string }, Cmd.none )

        EditLabel string ->
            ( { model | label = string }, Cmd.none )

        ToggleIndicating ->
            let
                newIndicating =
                    not model.indicating
            in
            ( { model
                | indicating = newIndicating
                , label =
                    if newIndicating then
                        model.label

                    else
                        "Uploading Files"
              }
                |> updatelabelOnIndicating
            , Cmd.none
            )

        ChangeState state ->
            ( { model
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
                            model.label
              }
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
    let
        controller =
            labeledButton []
                [ button [ onClick ProgressMinus ] [ text "-" ]
                , basicLabel [] [ text (String.fromFloat model.progressValue ++ "%") ]
                , button [ onClick ProgressPlus ] [ text "+" ]
                ]
    in
    skeleton model
        { changeThemeMsg = ChangeTheme }
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
            , configs =
                [ { label = "Bar"
                  , fields =
                        [ { label = ""
                          , description = "A progress element can contain a bar visually indicating progress"
                          , content = controller
                          }
                        ]
                  }
                , { label = "Types"
                  , fields =
                        [ { label = ""
                          , description = "An indicating progress bar visually indicates the current level of progress of a task"
                          , content =
                                checkbox
                                    { id = "indicating"
                                    , label = "Indicating"
                                    , checked = model.indicating
                                    , onClick = ToggleIndicating
                                    }
                          }
                        ]
                  }
                , { label = "States"
                  , fields =
                        [ { label = ""
                          , description =
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
                          , content =
                                select [ onInput (Progress.stateFromString >> Maybe.withDefault model.state >> ChangeState) ] <|
                                    List.map (\state -> option [ value (Progress.stateToString state), selected (model.state == state) ] [ text (Progress.stateToString state) ])
                                        [ Default, Active, Success, Warning, Error, Disabled ]
                          }
                        ]
                  }
                , { label = "Content"
                  , fields =
                        [ { label = "Progress"
                          , description = "A progress bar can contain a text value indicating current progress"
                          , content =
                                Input.input []
                                    [ input [ type_ "text", value model.progressLabel, onInput EditProgressLabel ] [] ]
                          }
                        , { label = "Label"
                          , description = "A progress element can contain a label"
                          , content =
                                Input.input []
                                    [ input [ type_ "text", value model.label, onInput EditLabel ] [] ]
                          }
                        ]
                  }
                ]
            }
        ]
