module Page.Breadcrumb exposing (Model, Msg, init, update, view)

import Config
import ConfigAndPreview exposing (configAndPreview)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html)
import Types exposing (Size(..), sizeFromString, sizeToString)
import UI.Breadcrumb exposing (Divider(..), bigBreadCrumb, dividerFromString, dividerToString, hugeBreadCrumb, largeBreadCrumb, massiveBreadCrumb, mediumBreadCrumb, miniBreadCrumb, smallBreadCrumb, tinyBreadCrumb)



-- INIT


type alias Model =
    { theme : Theme
    , divider : Divider
    , size : Size
    }


init : ( Model, Cmd Msg )
init =
    ( { theme = System
      , divider = Slash
      , size = Medium
      }
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


view : Model -> List (Html Msg)
view model =
    let
        options =
            { divider = model.divider, theme = model.theme }
    in
    [ let
        breadcrumb_ =
            case model.size of
                Mini ->
                    miniBreadCrumb

                Tiny ->
                    tinyBreadCrumb

                Small ->
                    smallBreadCrumb

                Medium ->
                    mediumBreadCrumb

                Large ->
                    largeBreadCrumb

                Big ->
                    bigBreadCrumb

                Huge ->
                    hugeBreadCrumb

                Massive ->
                    massiveBreadCrumb
      in
      configAndPreview UpdateConfig { theme = model.theme } <|
        { title = "Breadcrumb"
        , preview =
            [ breadcrumb_ options
                [ { label = "Home", url = "/" }
                , { label = "Store", url = "/" }
                , { label = "T-Shirt", url = "" }
                ]
            ]
        , configSections =
            [ { label = "Content"
              , configs =
                    [ { label = "Divider"
                      , config =
                            Config.select
                                { value = model.divider
                                , options = [ Slash, RightChevron ]
                                , fromString = dividerFromString
                                , toString = dividerToString
                                , setter = \divider m -> { m | divider = divider }
                                }
                      , note = "A breadcrumb can contain a divider to show the relationship between sections, this can be formatted as an icon or text."
                      }
                    ]
              }
            , { label = "Variations"
              , configs =
                    [ { label = ""
                      , config =
                            Config.bool
                                { id = "inverted"
                                , label = "Inverted"
                                , bool = model.theme == Dark
                                , setter =
                                    \m ->
                                        { m
                                            | theme =
                                                case m.theme of
                                                    System ->
                                                        Dark

                                                    Light ->
                                                        Dark

                                                    Dark ->
                                                        Light
                                        }
                                }
                      , note = "A breadcrumb can be inverted"
                      }
                    , { label = "Size"
                      , config =
                            Config.select
                                { value = model.size
                                , options = [ Mini, Tiny, Small, Medium, Large, Big, Huge, Massive ]
                                , fromString = sizeFromString
                                , toString = sizeToString
                                , setter = \size m -> { m | size = size }
                                }
                      , note = "A breadcrumb can vary in size"
                      }
                    ]
              }
            ]
        }
    ]
