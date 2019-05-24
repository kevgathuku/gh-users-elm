module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, list, map3, string)


type alias Member =
    { login : String
    , avatar_url : String
    , html_url : String
    }


type alias Members =
    List Member


type alias Model =
    { org : String
    , members : Members
    , status : GithubRequestStatus
    }


type GithubRequestStatus
    = NotRequested
    | Loading
    | Failure
    | Success


initialModel : Model
initialModel =
    { org = "andela", members = [], status = NotRequested }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = RequestOrgUsers
    | GotMembers (Result Http.Error Members)


fetchOrgUsers : String -> Cmd Msg
fetchOrgUsers org =
    Http.get
        { url = "https://api.github.com/orgs/" ++ org ++ "/members"
        , expect = Http.expectJson GotMembers membersDecoder
        }


membersDecoder : Decoder Members
membersDecoder =
    list memberDecoder


memberDecoder : Decoder Member
memberDecoder =
    map3 Member
        (field "login" string)
        (field "avatar_url" string)
        (field "html_url" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestOrgUsers ->
            ( { model | status = Loading }
            , fetchOrgUsers model.org
            )

        GotMembers result ->
            case result of
                Ok members ->
                    ( { model | status = Success, members = members }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )


tableRow : Member -> Html Msg
tableRow member =
    tr []
        [ td []
            [ img [ class "avatar", src member.avatar_url, alt (member.login ++ " avatar") ] []
            ]
        , td []
            [ a [ href member.html_url, target "_blank" ] [ text member.login ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm GH Users app"
    , body =
        [ div [ class "container" ]
            [ h1 [] [ text "Hello from Elm GitHub Users App" ]
            , if model.status == Success then
                (text "")
             else
                button [ onClick RequestOrgUsers ] [ text "Request data" ]
            , table [ class "u-full-width" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Avatar" ]
                        , th [] [ text "Name" ]
                        ]
                    ]
                , tbody [] (List.map tableRow model.members)
                ]
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
