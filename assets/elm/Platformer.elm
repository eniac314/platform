module Platformer exposing (..)

import Html exposing (Html, div)
import Keyboard exposing (KeyCode, downs)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame exposing (diffs)
import Random
import Time exposing (Time, second, every)
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Json.Encode as Encode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ downs KeyDown
        , diffs TimeUpdate
        , every Time.second CountDownTimer
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        ]



-- Model


type GameState
    = StartScreen
    | Playing
    | Success
    | GameOver


type Direction
    = Left
    | Right
    | Eating


type alias Model =
    { posX : Int
    , posY : Int
    , gameState : GameState
    , itemPosX : Int
    , itemPosY : Int
    , direction : Direction
    , itemsCollected : Int
    , score : Int
    , timeRemaining : Int
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type Msg
    = NoOp
    | KeyDown KeyCode
    | TimeUpdate Time
    | CountDownTimer Time
    | SetNewItemPosX Int
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SaveScoreRequest
    | SaveScore Encode.Value
    | SaveScoreError Encode.Value


initialModel : Model
initialModel =
    { posX = 1
    , posY = 300
    , gameState = StartScreen
    , itemPosX = 150
    , itemPosY = 315
    , direction = Right
    , itemsCollected = 0
    , score = 0
    , timeRemaining = 10
    , phxSocket = initialSocket
    }


initialSocket : Phoenix.Socket.Socket Msg
initialSocket =
    let
        devSocketServer =
            "ws://localhost:4000/socket/websocket"
    in
        Phoenix.Socket.init devSocketServer
            |> Phoenix.Socket.withDebug


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                39 ->
                    if model.gameState == Playing then
                        { model
                            | posX = model.posX + 15
                            , direction = Right
                        }
                            ! []
                    else
                        model ! []

                37 ->
                    if model.gameState == Playing then
                        { model
                            | posX = model.posX - 15
                            , direction = Left
                        }
                            ! []
                    else
                        model ! []

                32 ->
                    if model.gameState /= Playing then
                        { model
                            | gameState = Playing
                            , direction = Right
                            , posX = 50
                            , itemsCollected = 0
                            , score = 0
                            , timeRemaining = 10
                        }
                            ! []
                    else
                        model ! []

                _ ->
                    model ! []

        TimeUpdate time ->
            if characterFoundItem model then
                { model
                    | direction = Eating
                    , score = model.score + 5
                    , itemsCollected = model.itemsCollected + 1
                }
                    ! [ Random.generate SetNewItemPosX (Random.int 75 450) ]
            else if model.itemsCollected >= 10 then
                { model | gameState = Success } ! []
            else if model.itemsCollected < 10 && model.timeRemaining == 0 then
                ( { model | gameState = GameOver }, Cmd.none )
            else
                model ! []

        CountDownTimer time ->
            if model.timeRemaining > 0 && model.gameState == Playing then
                { model | timeRemaining = model.timeRemaining - 1 } ! []
            else
                model ! []

        SetNewItemPosX n ->
            { model | itemPosX = n } ! []

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                { model | phxSocket = phxSocket }
                    ! [ Cmd.map PhoenixMsg phxCmd ]

        SaveScoreRequest ->
            let
                payload =
                    Encode.object [ ( "player_score", Encode.int model.score ) ]

                phxPush =
                    Phoenix.Push.init "save_score" "score:platformer"
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onOk SaveScore
                        |> Phoenix.Push.onError SaveScoreError

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push phxPush model.phxSocket
            in
                { model | phxSocket = phxSocket } ! [ Cmd.map PhoenixMsg phxCmd ]

        SaveScore value ->
            model ! []

        SaveScoreError value ->
            model ! []


characterFoundItem : Model -> Bool
characterFoundItem model =
    let
        approximateItemLowerBound =
            model.itemPosX - 35

        approximateItemUpperBound =
            model.itemPosX

        approximateItemRange =
            List.range approximateItemLowerBound approximateItemUpperBound
    in
        List.member model.posX approximateItemRange



--View


view : Model -> Html Msg
view model =
    div [] [ viewGame model ]


viewGame : Model -> Svg Msg
viewGame model =
    svg [ version "1.1", width "600", height "400" ]
        (viewGameState model)


viewGameState : Model -> List (Svg Msg)
viewGameState model =
    case model.gameState of
        StartScreen ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewCharacter model
            , viewItem model
            , viewStartScreenText
            ]

        Playing ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewCharacter model
            , viewItem model
            , viewGameScore model
            , viewItemsCollected model
            , viewGameTime model
            ]

        Success ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewCharacter model
            , viewItem model
            , viewSuccessScreenText
            ]

        GameOver ->
            [ viewGameWindow
            , viewGameSky
            , viewGameGround
            , viewCharacter model
            , viewItem model
            , viewGameOverScreenText
            ]


viewStartScreenText : Svg Msg
viewStartScreenText =
    Svg.svg []
        [ viewGameText 140 160 "Collect ten coins in ten seconds!"
        , viewGameText 140 180 "Press the SPACE BAR key to start."
        ]


viewGameText : Int -> Int -> String -> Svg Msg
viewGameText posX posY str =
    Svg.text_
        [ x (toString posX)
        , y (toString posY)
        , fontFamily "Courier"
        , fontWeight "bold"
        , fontSize "16"
        ]
        [ Svg.text str ]


viewGameScore : Model -> Svg Msg
viewGameScore model =
    let
        currentScore =
            model.score
                |> toString
                |> String.padLeft 5 '0'
    in
        Svg.svg []
            [ viewGameText 25 25 "SCORE"
            , viewGameText 25 40 currentScore
            ]


viewItemsCollected : Model -> Svg Msg
viewItemsCollected model =
    let
        currentItemCount =
            model.itemsCollected
                |> toString
                |> String.padLeft 3 '0'
    in
        Svg.svg []
            [ image
                [ xlinkHref "/images/brain.png"
                , x "275"
                , y "18"
                , width "15"
                , height "15"
                ]
                []
            , viewGameText 300 30 ("x " ++ currentItemCount)
            ]


viewGameTime : Model -> Svg Msg
viewGameTime model =
    let
        currentTime =
            model.timeRemaining
                |> toString
                |> String.padLeft 4 '0'
    in
        Svg.svg []
            [ viewGameText 525 25 "TIME"
            , viewGameText 525 40 currentTime
            ]


viewGameWindow : Svg Msg
viewGameWindow =
    rect
        [ width "600"
        , height "400"
        , fill "none"
        , stroke "black"
        ]
        []


viewGameSky : Svg Msg
viewGameSky =
    rect
        [ x "0"
        , y "0"
        , width "600"
        , height "300"
        , fill "#4b7cfb"
        ]
        []


viewGameGround : Svg Msg
viewGameGround =
    rect
        [ x "0"
        , y "300"
        , width "600"
        , height "100"
        , fill "green"
        ]
        []


viewCharacter : Model -> Svg Msg
viewCharacter { posX, posY, direction } =
    let
        sprite =
            case direction of
                Right ->
                    "/images/totor.png"

                Left ->
                    "/images/totorLeft.png"

                Eating ->
                    "/images/totorEats.png"
    in
        image
            [ xlinkHref sprite
            , x (toString posX)
            , y (toString posY)
            , width "50"
            , height "50"
            ]
            []


viewItem : Model -> Svg Msg
viewItem { itemPosX, itemPosY } =
    image
        [ xlinkHref "/images/brain.png"
        , x (toString itemPosX)
        , y (toString itemPosY)
        , width "75"
        , height "75"
        ]
        []


viewSuccessScreenText : Svg Msg
viewSuccessScreenText =
    Svg.svg []
        [ viewGameText 260 160 "Success!"
        , viewGameText 140 180 "Press the SPACE BAR key to restart."
        ]


viewGameOverScreenText : Svg Msg
viewGameOverScreenText =
    Svg.svg []
        [ viewGameText 260 160 "Game Over"
        , viewGameText 140 180 "Press the SPACE BAR key to restart."
        ]
