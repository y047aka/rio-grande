module Page.Progress exposing (Model, Msg, init, update, view)

import Config
import ConfigAndPreview exposing (configAndPreview)
import Css exposing (int)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html)
import Random
import Shared
import UI.Progress as Progress exposing (State(..))



-- MODEL


type alias Model =
    { progressValue : Float
    , progressLabel : String
    , label : String
    , indicating : Bool
    , state : State
    }


init : ( Model, Cmd Msg )
init =
    ( { progressValue = 0
      , progressLabel = "%"
      , label = "Uploading Files"
      , indicating = False
      , state = Default
      }
    , Random.generate NewProgress (Random.int 10 50)
    )



-- UPDATE


type Msg
    = NewProgress Int
    | CounterPlus
    | CounterMinus
    | UpdateConfig (Model -> Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        CounterPlus ->
            ( model, Random.generate NewProgress (Random.int 10 15) )

        CounterMinus ->
            ( model, Random.generate NewProgress (Random.int -15 -10) )

        UpdateConfig updater ->
            ( updater model, Cmd.none )


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


view : Shared.Model -> Model -> List (Html Msg)
view shared model =
    [ configAndPreview
        { title = "Progress"
        , theme = shared.theme
        , inverted = False
        , preview =
            [ Progress.progressWithProps
                { value = model.progressValue
                , progress = String.fromFloat model.progressValue ++ model.progressLabel
                , label = model.label
                , indicating = model.indicating
                , state = model.state
                }
            ]
        , configSections =
            [ { label = "Bar"
              , configs =
                    [ Config.counter
                        { label = ""
                        , value = model.progressValue
                        , toString = \value -> String.fromFloat value ++ "%"
                        , onClickPlus = CounterPlus
                        , onClickMinus = CounterMinus
                        , note = "A progress element can contain a bar visually indicating progress"
                        }
                    ]
              }
            , { label = "Types"
              , configs =
                    [ Config.bool
                        { id = "indicating"
                        , label = "Indicating"
                        , bool = model.indicating
                        , setter =
                            (\m ->
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
                            )
                                |> UpdateConfig
                        , note = "An indicating progress bar visually indicates the current level of progress of a task"
                        }
                    ]
              }
            , { label = "States"
              , configs =
                    [ Config.select
                        { label = ""
                        , value = model.state
                        , options = [ Default, Active, Success, Warning, Error, Disabled ]
                        , fromString = Progress.stateFromString
                        , toString = Progress.stateToString
                        , setter =
                            (\state m ->
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
                            )
                                >> UpdateConfig
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
                    [ Config.string
                        { label = "Progress"
                        , value = model.progressLabel
                        , setter = (\string m -> { m | progressLabel = string }) >> UpdateConfig
                        , note = "A progress bar can contain a text value indicating current progress"
                        }
                    , Config.string
                        { label = "Label"
                        , value = model.label
                        , setter = (\string m -> { m | label = string }) >> UpdateConfig
                        , note = "A progress element can contain a label"
                        }
                    ]
              }
            ]
        }
    ]
