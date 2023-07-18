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
    , page : PageModel
    , shared : Shared.Model
    }


type PageModel
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
            Model url key None shared
                |> routing url
    in
    ( model
    , Cmd.batch
        [ Cmd.map Shared sharedCmd
        , cmd
        ]
    )



-- ROUTER


routing : Url -> Model -> ( Model, Cmd Msg )
routing url model =
    Url.Parser.parse parser url
        |> Maybe.withDefault ( None, Effect.none )
        |> (\( pageModel, effect ) ->
                ( { model
                    | url = url
                    , page = pageModel
                  }
                , Effect.toCmd ( Shared, Page ) effect
                )
           )


parser : Parser (( PageModel, Effect PageMsg ) -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.top |> Url.Parser.map ( TopModel, Effect.none )
        , s "form"
            |> Url.Parser.map
                (Form.init
                    |> Tuple.mapSecond Effect.fromCmd
                    |> updateWith FormModel FormMsg
                )
        , s "progress"
            |> Url.Parser.map
                (Progress.init
                    |> Tuple.mapSecond Effect.fromCmd
                    |> updateWith ProgressModel ProgressMsg
                )
        , s "card"
            |> Url.Parser.map
                (Card.init
                    |> Tuple.mapSecond Effect.fromCmd
                    |> updateWith CardModel CardMsg
                )
        ]



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
                ( pageModel, effect ) =
                    case ( model.page, pageMsg ) of
                        ( FormModel pageModel_, FormMsg pageMsg_ ) ->
                            Form.update pageMsg_ pageModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith FormModel FormMsg

                        ( ProgressModel pageModel_, ProgressMsg pageMsg_ ) ->
                            Progress.update pageMsg_ pageModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith ProgressModel ProgressMsg

                        ( CardModel pageModel_, CardMsg pageMsg_ ) ->
                            Card.update pageMsg_ pageModel_
                                |> Tuple.mapSecond Effect.fromCmd
                                |> updateWith CardModel CardMsg

                        _ ->
                            ( None, Effect.none )
            in
            ( { model | page = pageModel }
            , Effect.toCmd ( Shared, Page ) effect
            )


updateWith : (pageModel -> PageModel) -> (pageMsg_ -> PageMsg) -> ( pageModel, Effect pageMsg_ ) -> ( PageModel, Effect PageMsg )
updateWith toModel toMsg ( pageModel, pageEffect ) =
    ( toModel pageModel, Effect.map toMsg pageEffect )



-- VIEW


view : Model -> Document Msg
view { url, shared, page } =
    (case page of
        None ->
            [ text "Not Found" ]

        TopModel ->
            [ a [ href "/form" ] [ text "Form" ]
            , a [ href "/progress" ] [ text "Progress" ]
            , a [ href "/card" ] [ text "Card" ]
            ]

        FormModel pageModel ->
            Form.view shared pageModel
                |> List.map (Html.map (FormMsg >> Page))

        ProgressModel pageModel ->
            Progress.view shared pageModel
                |> List.map (Html.map (ProgressMsg >> Page))

        CardModel pageModel ->
            Card.view shared pageModel
                |> List.map (Html.map (CardMsg >> Page))
    )
        |> (\view_ ->
                { title = "rio grande"
                , body =
                    [ skeleton { url = url, theme = shared.theme, changeThemeMsg = Shared.ChangeTheme >> Shared }
                        { title =
                            case page of
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
