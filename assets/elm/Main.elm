module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


type alias Model =
    { gamesList : List Game
    , playersList : List Player
    , errors : String
    }


type alias Game =
    { title : String
    , description : String
    , featured : Bool
    , id : Int
    , thumbnail : String
    , slug : String
    }


type alias Player =
    { displayName : Maybe String
    , id : Int
    , score : Int
    , username : String
    }


type Msg
    = FetchGamesList (Result Http.Error (List Game))
    | FetchPlayersList (Result Http.Error (List Player))


initialModel : Model
initialModel =
    { gamesList = []
    , playersList = []
    , errors = ""
    }


initialCommmand : Cmd Msg
initialCommmand =
    Cmd.batch
        [ fetchGamesList
        , fetchPlayersList
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( initialModel, initialCommmand )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGamesList res ->
            case res of
                Ok games ->
                    ( { model | gamesList = games }, Cmd.none )

                Err e ->
                    { model | errors = toString e } ! []

        FetchPlayersList res ->
            case res of
                Ok players ->
                    { model | playersList = players } ! []

                Err e ->
                    { model | errors = toString e } ! []


view : Model -> Html Msg
view model =
    if List.isEmpty model.gamesList then
        div [] []
    else
        div []
            [ featured model
            , gamesIndex model
            , playersIndex model
            ]


featured : Model -> Html Msg
featured model =
    case featuredGame model.gamesList of
        Just game ->
            div [ class "row featured" ]
                [ div [ class "container" ]
                    [ div [ class "featured-img" ]
                        [ img [ class "featured-thumbnail", src game.thumbnail ] [] ]
                    , div [ class "featured-data" ]
                        [ h1 [] [ text "Featured" ]
                        , h2 [] [ text game.title ]
                        , p [] [ text game.description ]
                        , a
                            [ class "btn btn-lg btn-primary"
                            , href <| "games/" ++ game.slug
                            ]
                            [ text "Play Now!" ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


gamesIndex : Model -> Html msg
gamesIndex model =
    if List.isEmpty model.gamesList then
        div [] []
    else
        div [ class "games-index" ]
            [ h1 [ class "games-section" ] [ text "Games" ]
            , gamesList model.gamesList
            ]


gamesList : List Game -> Html msg
gamesList games =
    ul [ class "games-list media-list" ] (List.map gamesListItem games)


gamesListItem : Game -> Html msg
gamesListItem game =
    a [ href <| "games/" ++ game.slug ]
        [ li [ class "game-item media" ]
            [ div [ class "media-left" ]
                [ img [ class "media-object", src game.thumbnail ] []
                ]
            , div [ class "media-body media-middle" ]
                [ h4 [ class "media-heading" ] [ text game.title ]
                , p [] [ text game.description ]
                ]
            ]
        ]


playersIndex : Model -> Html msg
playersIndex model =
    if List.isEmpty model.playersList then
        div [] []
    else
        div [ class "players-index" ]
            [ h1 [ class "players-section" ] [ text "Players" ]
            , model.playersList
                |> List.sortBy .score
                |> List.reverse
                |> playersList
            ]


playersList : List Player -> Html msg
playersList players =
    div [ class "players-list panel panel-info" ]
        [ div [ class "panel-heading" ] [ text "Leaderboard" ]
        , ul [ class "list-group" ] (List.map playersListItem players)
        ]


playersListItem : Player -> Html msg
playersListItem player =
    let
        displayName =
            Maybe.withDefault player.username player.displayName

        playerLink =
            "players/" ++ toString player.id
    in
    li [ class "player-item list-group-item" ]
        [ strong []
            [ a [ href playerLink ] [ text displayName ]
            ]
        , span [ class "badge" ] [ text (toString player.score) ]
        ]


featuredGame : List Game -> Maybe Game
featuredGame games =
    games
        |> List.filter .featured
        |> List.head


fetchGamesList =
    Http.get "/api/games" decodeGamesList
        |> Http.send FetchGamesList


fetchPlayersList =
    Http.get "/api/players" decodePlayersList
        |> Http.send FetchPlayersList


decodeGame : Decode.Decoder Game
decodeGame =
    Decode.map6 Game
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "featured" Decode.bool)
        (Decode.field "id" Decode.int)
        (Decode.field "thumbnail" Decode.string)
        (Decode.field "slug" Decode.string)


decodeGamesList : Decode.Decoder (List Game)
decodeGamesList =
    decodeGame
        |> Decode.list
        |> Decode.at [ "data" ]


decodePlayer : Decode.Decoder Player
decodePlayer =
    Decode.map4 Player
        (Decode.maybe (Decode.field "display_name" Decode.string))
        (Decode.field "id" Decode.int)
        (Decode.field "score" Decode.int)
        (Decode.field "username" Decode.string)


decodePlayersList : Decode.Decoder (List Player)
decodePlayersList =
    decodePlayer
        |> Decode.list
        |> Decode.at [ "data" ]
