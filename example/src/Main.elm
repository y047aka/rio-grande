module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Effect exposing (Effect)
import Html.Styled as Html exposing (a, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Page.Card as Card
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
    | FormModel Form.Model
    | ProgressModel Progress.Model
    | CardModel Card.Model


init : Shared.Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( shared, sharedCmd ) =
            Shared.init {} flags

        ( model, cmd ) =
            Model url key shared None
                |> routing url
    in
    ( model
    , Cmd.batch
        [ Cmd.map Shared sharedCmd
        , cmd
        ]
    )



-- ROUTER


type Page
    = NotFound
    | Top
    | Form
    | Progress
    | Card


parser : Parser (Page -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map Form (s "form")
        , Url.Parser.map Progress (s "progress")
        , Url.Parser.map Card (s "card")
        ]


routing : Url -> Model -> ( Model, Cmd Msg )
routing url model =
    Url.Parser.parse parser url
        |> Maybe.withDefault NotFound
        |> (\page ->
                let
                    ( subModel, effect ) =
                        case page of
                            NotFound ->
                                ( None, Effect.none )

                            Top ->
                                ( TopModel, Effect.none )

                            Form ->
                                Form.init
                                    |> Tuple.mapSecond Effect.fromCmd
                                    |> updateWith FormModel FormMsg

                            Progress ->
                                Progress.init
                                    |> Tuple.mapSecond Effect.fromCmd
                                    |> updateWith ProgressModel ProgressMsg

                            Card ->
                                Card.init
                                    |> Tuple.mapSecond Effect.fromCmd
                                    |> updateWith CardModel CardMsg
                in
                ( { model
                    | url = url
                    , subModel = subModel
                  }
                , Effect.toCmd ( Shared, Page ) effect
                )
           )



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Page PageMsg


type PageMsg
    = FormMsg Form.Msg
    | ProgressMsg Progress.Msg
    | CardMsg Card.Msg


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
            let
                ( subModel, effect ) =
                    case ( model.subModel, pageMsg ) of
                        ( FormModel subModel_, FormMsg subMsg ) ->
                            Form.update subMsg subModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith FormModel FormMsg

                        ( ProgressModel subModel_, ProgressMsg subMsg ) ->
                            Progress.update subMsg subModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith ProgressModel ProgressMsg

                        ( CardModel subModel_, CardMsg subMsg ) ->
                            Card.update subMsg subModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith CardModel CardMsg

                        _ ->
                            ( None, Effect.none )
            in
            ( { model | subModel = subModel }
            , Effect.toCmd ( Shared, Page ) effect
            )


updateWith : (subModel -> SubModel) -> (subMsg -> PageMsg) -> ( subModel, Effect subMsg ) -> ( SubModel, Effect PageMsg )
updateWith toModel toMsg ( subModel, subEffect ) =
    ( toModel subModel, Effect.map toMsg subEffect )



-- VIEW


view : Model -> Document Msg
view { url, shared, subModel } =
    (case subModel of
        None ->
            [ text "Not Found" ]

        TopModel ->
            [ a [ href "/form" ] [ text "Form" ]
            , a [ href "/progress" ] [ text "Progress" ]
            , a [ href "/card" ] [ text "Card" ]
            ]

        FormModel subModel_ ->
            List.map (Html.map FormMsg) (Form.view shared subModel_)

        ProgressModel subModel_ ->
            List.map (Html.map ProgressMsg) (Progress.view shared subModel_)

        CardModel subModel_ ->
            List.map (Html.map CardMsg) (Card.view shared subModel_)
    )
        |> List.map (Html.map Page)
        |> (\view_ ->
                { title = "rio grande"
                , body =
                    [ skeleton { url = url, theme = shared.theme, changeThemeMsg = Shared.ChangeTheme >> Shared }
                        { title =
                            case subModel of
                                FormModel _ ->
                                    "Form"

                                ProgressModel _ ->
                                    "Progress"

                                CardModel _ ->
                                    "Card"

                                _ ->
                                    ""
                        , body = view_
                        }
                        |> toUnstyled
                    ]
                }
           )
