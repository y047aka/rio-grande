module Skeleton exposing (skeleton)

import Css exposing (backgroundColor, borderBottom3, displayFlex, hex, int, justifyContent, padding, position, px, solid, spaceBetween, sticky, top, zIndex, zero)
import Css.FontAwesome exposing (fontAwesome)
import Css.Global exposing (global)
import Css.Reset exposing (normalize)
import Css.ResetAndCustomize exposing (additionalReset, globalCustomize)
import Data.Theme as Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, div, header, main_, option, select, text)
import Html.Styled.Attributes exposing (css, selected, value)
import Html.Styled.Events exposing (onInput)
import UI.Breadcrumb exposing (BreadcrumbItem, Divider(..), breadcrumbWithProps)
import UI.Container exposing (container)
import UI.Segment exposing (basicSegment)


skeleton : { model | theme : Theme } -> { changeThemeMsg : Theme -> msg } -> List (Html msg) -> Html msg
skeleton model { changeThemeMsg } body =
    div []
        [ global (normalize ++ additionalReset ++ globalCustomize ++ fontAwesome)
        , siteHeader model { changeThemeMsg = changeThemeMsg } { title = "title" }
        , main_ [] [ basicSegment { theme = Light } [] [ container [] body ] ]
        ]


siteHeader : { model | theme : Theme } -> { changeThemeMsg : Theme -> msg } -> { title : String } -> Html msg
siteHeader model { changeThemeMsg } page =
    header
        [ css
            [ position sticky
            , top zero
            , zIndex (int 1)
            , displayFlex
            , justifyContent spaceBetween
            , padding (px 20)
            , backgroundColor (hex "#FFF")
            , borderBottom3 (px 1) solid (hex "#EEE")
            ]
        ]
        [ breadcrumbWithProps { divider = Slash, size = Nothing, theme = model.theme }
            (breadcrumbItems page)
        , div []
            [ select [ onInput (Theme.fromString >> Maybe.withDefault model.theme >> (\theme -> changeThemeMsg theme)) ] <|
                List.map (\theme -> option [ value (Theme.toString theme), selected (model.theme == theme) ] [ text (Theme.toString theme) ])
                    [ System, Light, Dark ]
            ]
        ]


breadcrumbItems : { title : String } -> List BreadcrumbItem
breadcrumbItems { title } =
    case "/" of
        "/" ->
            [ { label = "Top", url = "/" } ]

        _ ->
            [ { label = "Top", url = "/" }
            , { label = title, url = "" }
            ]
