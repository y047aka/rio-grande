module Shared exposing (Flags, Model, Msg(..), init, subscriptions, update)

import Data.Theme exposing (Theme(..))
import Json.Decode as Json


type alias Flags =
    Json.Value


type alias Model =
    { theme : Theme }


type Msg
    = ChangeTheme Theme


init : request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { theme = System }, Cmd.none )


update : request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )


subscriptions : request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
