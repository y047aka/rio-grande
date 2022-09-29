module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Html.Styled as Html exposing (a, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Page.Breadcrumb as Breadcrumb
import Page.Form as Form
import Page.Progress as Progress
import Shared
import Skeleton exposing (skeleton)
import Url exposing (Url)
import Url.Parser exposing (Parser, s)


main : Program Shared.Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { url : Url
    , key : Key
    , shared : Shared.Model
    , subModel : SubModel
    }


type SubModel
    = None
    | TopModel
    | BreadcrumbModel Breadcrumb.Model
    | FormModel Form.Model
    | ProgressModel Progress.Model


init : Shared.Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( shared, _ ) =
            Shared.init {} flags
    in
    { url = url
    , key = key
    , shared = shared
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
                            |> updateWith BreadcrumbModel (BreadcrumbMsg >> Page) model

                    Form ->
                        Form.init
                            |> updateWith FormModel (FormMsg >> Page) model

                    Progress ->
                        Progress.init
                            |> updateWith ProgressModel (ProgressMsg >> Page) model
           )



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Page PageMsg


type PageMsg
    = BreadcrumbMsg Breadcrumb.Msg
    | FormMsg Form.Msg
    | ProgressMsg Progress.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            routing url model

        Shared sharedMsg ->
            let
                ( shared, sharedCmd ) =
                    Shared.update {} sharedMsg model.shared
            in
            ( { model | shared = shared }
            , Cmd.map Shared sharedCmd
            )

        Page pageMsg ->
            case ( model.subModel, pageMsg ) of
                ( BreadcrumbModel subModel, BreadcrumbMsg subMsg ) ->
                    Breadcrumb.update subMsg subModel
                        |> updateWith BreadcrumbModel (BreadcrumbMsg >> Page) model

                ( FormModel subModel, FormMsg subMsg ) ->
                    Form.update subMsg subModel
                        |> updateWith FormModel (FormMsg >> Page) model

                ( ProgressModel subModel, ProgressMsg subMsg ) ->
                    Progress.update subMsg subModel
                        |> updateWith ProgressModel (ProgressMsg >> Page) model

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
    (case model.subModel of
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
    )
        |> List.map (Html.map Page)
        |> (\view_ ->
                { title = "rio grande"
                , body =
                    [ skeleton { url = model.url, theme = model.shared.theme, changeThemeMsg = Shared.ChangeTheme >> Shared }
                        { title =
                            case model.subModel of
                                BreadcrumbModel _ ->
                                    "Breadcrumb"

                                FormModel _ ->
                                    "Form"

                                ProgressModel _ ->
                                    "Progress"

                                _ ->
                                    ""
                        , body = view_
                        }
                        |> toUnstyled
                    ]
                }
           )
