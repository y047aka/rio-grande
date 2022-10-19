module Page.Breadcrumb exposing (Model, Msg, init, update, view)

import Config
import ConfigAndPreview exposing (configAndPreview)
import Data.Theme exposing (Theme(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Shared
import Types exposing (Size(..), sizeFromString, sizeToString)
import UI.Breadcrumb exposing (Divider(..), bigBreadCrumb, dividerFromString, dividerToString, hugeBreadCrumb, largeBreadCrumb, massiveBreadCrumb, mediumBreadCrumb, miniBreadCrumb, smallBreadCrumb, tinyBreadCrumb)



-- INIT


type alias Model =
    { divider : Divider
    , inverted : Bool
    , size : Size
    }


init : ( Model, Cmd Msg )
init =
    ( { divider = Slash
      , inverted = False
      , size = Medium
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateConfig (Config.Msg Model)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateConfig configMsg ->
            ( Config.update configMsg model, Effect.none )



-- VIEW


view : Shared.Model -> Model -> List (Html Msg)
view shared model =
    let
        options =
            { divider = model.divider, theme = shared.theme }
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
      configAndPreview UpdateConfig { theme = shared.theme, inverted = model.inverted } <|
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
                                , bool = model.inverted
                                , setter = \m -> { m | inverted = not m.inverted }
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
