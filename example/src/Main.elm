module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (a, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Page.Breadcrumb as Breadcrumb
import Page.Form as Form
import Page.Progress as Progress
import Skeleton exposing (skeleton)
import Url exposing (Url)
import Url.Parser exposing (Parser, s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- INIT


type alias Model =
    { key : Key
    , theme : Theme
    , subModel : SubModel
    }


type SubModel
    = None
    | TopModel
    | BreadcrumbModel Breadcrumb.Model
    | FormModel Form.Model
    | ProgressModel Progress.Model


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    { key = key
    , theme = System
    , subModel = None
    }
        |> routing url



-- ROUTER


type Page
    = NotFound
    | Top
    | Breadcrumb
    | Form
    | Progress


parser : Parser (Page -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map Breadcrumb (s "breadcrumb")
        , Url.Parser.map Form (s "form")
        , Url.Parser.map Progress (s "progress")
        ]


routing : Url -> Model -> ( Model, Cmd Msg )
routing url model =
    Url.Parser.parse parser url
        |> Maybe.withDefault NotFound
        |> (\page ->
                case page of
                    NotFound ->
                        ( { model | subModel = None }, Cmd.none )

                    Top ->
                        ( { model | subModel = TopModel }, Cmd.none )

                    Breadcrumb ->
                        Breadcrumb.init
                            |> updateWith BreadcrumbModel BreadcrumbMsg model

                    Form ->
                        Form.init
                            |> updateWith FormModel FormMsg model

                    Progress ->
                        Progress.init
                            |> updateWith ProgressModel ProgressMsg model
           )



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ChangeTheme Theme
    | BreadcrumbMsg Breadcrumb.Msg
    | FormMsg Form.Msg
    | ProgressMsg Progress.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.subModel, msg ) of
        ( _, UrlRequested urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            routing url model

        ( _, ChangeTheme theme ) ->
            ( { model | theme = theme }, Cmd.none )

        ( BreadcrumbModel subModel, BreadcrumbMsg subMsg ) ->
            Breadcrumb.update subMsg subModel
                |> updateWith BreadcrumbModel BreadcrumbMsg model

        ( FormModel subModel, FormMsg subMsg ) ->
            Form.update subMsg subModel
                |> updateWith FormModel FormMsg model

        ( ProgressModel subModel, ProgressMsg subMsg ) ->
            Progress.update subMsg subModel
                |> updateWith ProgressModel ProgressMsg model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> SubModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | subModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "rio grande"
    , body =
        List.map toUnstyled <|
            [ skeleton { theme = model.theme, changeThemeMsg = ChangeTheme } <|
                case model.subModel of
                    None ->
                        [ text "Not Found" ]

                    TopModel ->
                        [ a [ href "/breadcrumb" ] [ text "Breadcrumb" ]
                        , a [ href "/form" ] [ text "Form" ]
                        , a [ href "/progress" ] [ text "Progress" ]
                        ]

                    BreadcrumbModel subModel ->
                        List.map (Html.map BreadcrumbMsg) (Breadcrumb.view subModel)

                    FormModel subModel ->
                        List.map (Html.map FormMsg) (Form.view subModel)

                    ProgressModel subModel ->
                        List.map (Html.map ProgressMsg) (Progress.view subModel)
            ]
    }
