module Breadcrumb exposing (main)

import Browser
import Config exposing (Config(..))
import ConfigAndPreview exposing (configAndPreview)
import Data exposing (Size(..))
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, toUnstyled)
import Skeleton exposing (skeleton)
import UI.Breadcrumb exposing (Divider(..), bigBreadCrumb, breadcrumbWithProps, dividerFromString, dividerToString, hugeBreadCrumb, largeBreadCrumb, massiveBreadCrumb, mediumBreadCrumb, miniBreadCrumb, smallBreadCrumb, tinyBreadCrumb)
import UI.Segment exposing (segment)


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
    , divider : Divider
    , size : Size
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { theme = System
      , divider = Slash
      , size = Medium
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeTheme Theme
    | ChangeDivider Divider
    | ChangeSize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( { model | theme = theme }, Cmd.none )

        ChangeDivider divider ->
            ( { model | divider = divider }, Cmd.none )

        ChangeSize size ->
            ( { model | size = size }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        options =
            { divider = model.divider, theme = model.theme }
    in
    skeleton model
        { changeThemeMsg = ChangeTheme }
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
          configAndPreview
            { title = "Breadcrumb"
            , preview =
                [ breadcrumb_ options
                    [ { label = "Home", url = "/" }
                    , { label = "Store", url = "/" }
                    , { label = "T-Shirt", url = "" }
                    ]
                ]
            , configSets =
                [ { label = "Content"
                  , configs =
                        [ { label = "Divider"
                          , config =
                                Select
                                    { value = model.divider
                                    , options = [ Slash, RightChevron ]
                                    , fromString = dividerFromString
                                    , toString = dividerToString
                                    , onChange = ChangeDivider
                                    }
                          , note = "A breadcrumb can contain a divider to show the relationship between sections, this can be formatted as an icon or text."
                          }
                        ]
                  }

                -- , { label = "Variations"
                --   , configs =
                --         [ { label = "Size"
                --           , config =
                --                 Select
                --                     { value = model.size
                --                     , options = [ Mini, Tiny, Small, Medium, Large, Big, Huge, Massive ]
                --                     , fromString = sizeFromString
                --                     , toString = sizeToString
                --                     , onChange = ChangeSize
                --                     }
                --           , note = "A breadcrumb can vary in size"
                --           }
                --         ]
                --   }
                ]
            }
        , configAndPreview
            { title = "Inverted"
            , preview =
                [ segment { theme = Dark }
                    []
                    [ breadcrumbWithProps { divider = Slash, size = Nothing, theme = Dark }
                        [ { label = "Home", url = "/" }
                        , { label = "Registration", url = "/" }
                        , { label = "Personal Information", url = "" }
                        ]
                    ]
                ]
            , configSets = []
            }
        ]
