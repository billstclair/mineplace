--------------------------------------------------------------------
--
-- Main.elm
-- Mineplace.Server top-level application
-- Copyright (c) 2019-2025 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


port module Main exposing (main)

-- TODO: everything below here.

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Deck
import Dict exposing (Dict)
import Dict.Extra as DE
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
import Fifo exposing (Fifo)
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events exposing (keyCode, on)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import Markdown
import PortFunnel.LocalStorage as LocalStorage exposing (Label)
import PortFunnel.Notification as Notification exposing (Permission(..))
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..))
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.EncodeDecode as ED
import SayUncle.Interface as Interface
import SayUncle.Types as Types
    exposing
        ( AskYesNo(..)
        , Board
        , BoardClick(..)
        , ChatSettings
        , Choice(..)
        , ConnectionReason(..)
        , ConnectionSpec
        , Game
        , GameInterface
        , GameState
        , Message(..)
        , MessageQueueEntry
        , Model
        , Msg(..)
        , Page(..)
        , Participant
        , Player
        , PlayerNames
        , PublicGame
        , PublicGameAndPlayers
        , PublicType(..)
        , RowCol
        , SavedModel
        , Score
        , ServerState
        , Settings
        , State(..)
        , StatisticsKeys
        , Style
        , StyleType(..)
        , WinReason(..)
        , Winner(..)
        , statisticsKeys
        )
import SayUncle.UI as UI exposing (ids, onEnter, onKeydown, view)
import SayUncle.WhichServer as WhichServer
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , strokeDasharray
        , strokeWidth
        , textAnchor
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Svg.Button as SB exposing (Button, Content(..))
import Svg.Events
import Task
import Time exposing (Month(..), Posix, Zone)
import Url exposing (Url)
import WebSocketFramework
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , MessageDecoder
        , MessageEncoder
        , PlayerId
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , Statistics
        )


port onVisibilityChange : (Bool -> msg) -> Sub msg


port playAudio : String -> Cmd msg


isPlaying : Model -> Bool
isPlaying model =
    let
        game =
            model.game

        gameState =
            game.gameState
    in
    game.isLive && Dict.size gameState.players == gameState.maxPlayers


main =
    Browser.application
        { init = init
        , view = UI.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


view : Model -> Document Msg
view model =
    { title = "Say Uncle"
    , body =
        [ text "Say Uncle User Interface under construction."
        ]
    }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoderWithPrivate
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorMessageEncoder
    }


errorMessageEncoder : WebSocketFramework.Types.Error Message -> Message
errorMessageEncoder error =
    ErrorRsp
        { request = "Unknown"
        , text =
            Debug.toString error.kind
                ++ ":"
                ++ error.description
                ++ " / "
                ++ (case error.message of
                        Ok message ->
                            Debug.toString message

                        Err err ->
                            err
                   )
        }


fullProcessor : ServerMessageProcessor GameState Participant Message
fullProcessor =
    ServerInterface.fullMessageProcessor encodeDecode Interface.proxyMessageProcessor


updateServerState : (ServerState -> ServerState) -> GameInterface -> GameInterface
updateServerState updater serverInterface =
    let
        (ServerInterface interface) =
            serverInterface
    in
    case interface.state of
        Nothing ->
            let
                state =
                    WebSocketFramework.Types.emptyServerState Nothing
            in
            ServerInterface
                { interface
                    | state = Just <| updater state
                }

        Just state ->
            ServerInterface
                { interface
                    | state = Just <| updater state
                }


updateServerTime : Posix -> GameInterface -> GameInterface
updateServerTime posix serverInterface =
    serverInterface
        |> updateServerState (\state -> { state | time = posix })


updateServerSeed : Maybe Seed -> GameInterface -> GameInterface
updateServerSeed maybeSeed serverInterface =
    case maybeSeed of
        Nothing ->
            serverInterface

        Just seed ->
            serverInterface
                |> updateServerState (\state -> { state | seed = seed })


proxyServer : Maybe Seed -> GameInterface
proxyServer seed =
    ServerInterface.makeProxyServer fullProcessor (IncomingMessage True)
        |> updateServerSeed seed


updateChatAttributes : Int -> StyleType -> ChatSettings -> ChatSettings
updateChatAttributes bsize styleType settings =
    let
        renderStyle =
            Types.typeToStyle styleType

        attributes =
            settings.attributes
    in
    { settings
        | attributes =
            { attributes
                | chatTable =
                    [ style "width" "fit-content"
                    , style "max-width" "90%"
                    , style "margin" "auto"
                    ]
                , textColumn =
                    [ style "width" "fit-content"
                    ]
                , textArea =
                    [ style "width" <| String.fromInt (5 * bsize // 6) ++ "px"
                    , style "height" "6em"
                    , style "border-color" renderStyle.lineColor
                    ]
            }
    }


initialChatSettings : Zone -> ChatSettings
initialChatSettings zone =
    let
        settings =
            ElmChat.makeSettings ids.chatOutput 14 True ChatUpdate
    in
    { settings | zone = zone }


initialGame : Maybe Seed -> Game
initialGame maybeSeed =
    let
        seed =
            case maybeSeed of
                Just s ->
                    s

                Nothing ->
                    makeSeed zeroTick
    in
    { gameid = ""
    , playerIds = Dict.empty
    , gameState = Interface.emptyGameState Types.emptyPlayerNames 0 0 seed
    , isLocal = True
    , serverUrl = WhichServer.serverUrl
    , player = 0
    , playerid = ""
    , isLive = False

    -- not persistent
    , interfaceIsProxy = True
    , interface = proxyServer maybeSeed
    }


insertConnectionSpec : ConnectionSpec -> Model -> Model
insertConnectionSpec spec model =
    { model
        | connectionSpecQueue =
            Fifo.insert spec model.connectionSpecQueue
    }


removeConnectionSpec : Model -> ( Maybe ConnectionSpec, Model )
removeConnectionSpec model =
    let
        ( spec, queue ) =
            Fifo.remove model.connectionSpecQueue
    in
    ( spec, { model | connectionSpecQueue = queue } )


isConnectionSpecQueueEmpty : Model -> Bool
isConnectionSpecQueueEmpty model =
    model.connectionSpecQueue == Fifo.empty


zeroTick : Posix
zeroTick =
    Time.millisToPosix 0


makeSeed : Posix -> Seed
makeSeed posix =
    Random.initialSeed (Time.posixToMillis posix)


modelSeed : Model -> Maybe Seed
modelSeed model =
    if model.tick == zeroTick then
        Nothing

    else
        Just <| makeSeed model.tick


init : Value -> url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        game : Game
        game =
            initialGame Nothing

        model : Model
        model =
            { tick = zeroTick
            , zone = Time.utc
            , delayTime = zeroTick
            , game = game
            , gameDict = Dict.empty
            , chatSettings =
                initialChatSettings Time.utc
            , connectionSpecQueue = Fifo.empty
            , funnelState = initialFunnelState
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , error = Nothing
            , publicGames = []
            , requestedNew = False
            , reallyClearStorage = False
            , statistics = Nothing
            , statisticsTimes = ( Nothing, Nothing )
            , notificationAvailable = Nothing
            , notificationPermission = Nothing
            , visible = True
            , soundFile = Nothing
            , messageQueue = Fifo.empty
            , showMessageQueue = False

            -- persistent fields
            , page = MainPage
            , gameid = ""
            , settings = Types.emptySettings
            , styleType = LightStyle
            , notificationsEnabled = False
            , soundEnabled = False
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform SetZone Time.here
            , Task.perform Tick Time.now
            ]


type alias NewReqBody =
    { name : String
    , publicType : PublicType
    , maxPlayers : Int
    , winningPoints : Int
    , seedInt : Int
    , restoreState : Maybe GameState
    , maybeGameid : Maybe GameId
    }


initialNewReqBody : Int -> Int -> Posix -> NewReqBody
initialNewReqBody maxPlayers winningPoints posix =
    { name = ""
    , publicType = NotPublic
    , maxPlayers = maxPlayers
    , winningPoints = winningPoints
    , seedInt = Time.posixToMillis posix
    , restoreState = Nothing
    , maybeGameid = Nothing
    }


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if LocalStorage.isLoaded state.storage then
                        True

                    else
                        model.started
            }

        cmd =
            if mdl.started && not model.started && model.tick /= zeroTick then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys mdl

        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse label key v model

        _ ->
            mdl |> withCmd cmd


handleListKeysResponse : Label -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse label prefix keys model =
    case label of
        Nothing ->
            model |> withNoCmd

        Just lab ->
            if lab == pk.game then
                let
                    getter key cmd =
                        Cmd.batch
                            [ cmd
                            , getLabeled pk.game key
                            ]

                    getCmds =
                        List.foldr getter Cmd.none <| Debug.log "Getting games" keys
                in
                ( model, getCmds )

            else
                model |> withNoCmd


handleGetResponse : Label -> String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse label key value model =
    case label of
        Just lab ->
            if lab == pk.game then
                handleGetGameResponse key value model

            else if lab == pk.chat then
                handleGetChatResponse key value model

            else
                model |> withNoCmd

        Nothing ->
            if key == pk.model then
                let
                    cmd =
                        listKeysLabeled pk.game pk.game
                in
                case ED.decodeSavedModel value of
                    Err e ->
                        model |> withCmd cmd

                    Ok savedModel ->
                        savedModelToModel savedModel model
                            |> withCmd cmd

            else
                model |> withNoCmd


handleGetGameResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetGameResponse _ value model =
    let
        foo =
            Debug.log "handleGetGameResponse"
    in
    case
        JD.decodeValue (ED.gameDecoder <| proxyServer (modelSeed model))
            value
    of
        Err _ ->
            { model
                | error = Just <| "Couldn't decode game."
            }
                |> withNoCmd

        Ok game ->
            let
                model2 =
                    { model
                        | game = game
                    }

                model3 =
                    -- Empty chat in case it's not in localStorage
                    { model2
                        | chatSettings =
                            initialChatSettings model2.zone
                    }

                getChatCmd =
                    getChat

                ( model4, cmd4 ) =
                    reconnectToGame game model3
            in
            model4 |> withCmds [ getChatCmd, cmd4 ]


maybeRestoreSubscriptions : Model -> ( Model, Cmd Msg )
maybeRestoreSubscriptions model =
    let
        restoreSubscription connectionType =
            model
                |> webSocketConnect
                    model.game
                    (ConnectionSpec connectionType)
    in
    if model.page == PublicPage then
        restoreSubscription PublicGamesConnection

    else if model.page == StatisticsPage then
        restoreSubscription StatisticsConnection

    else
        model |> withNoCmd


reconnectToGame : Game -> Model -> ( Model, Cmd Msg )
reconnectToGame game model =
    if not game.isLocal && game.isLive && game.playerid /= "" then
        model
            |> webSocketConnect
                game
                (ConnectionSpec <| UpdateConnection game.playerid)

    else if game.isLocal then
        model
            |> withCmd (initialNewReqCmd game model)

    else
        -- Need to set gameid to ""?
        maybeRestoreSubscriptions model


updateChat : Model -> (ChatSettings -> ChatSettings) -> ( Model, ChatSettings )
updateChat model updater =
    let
        chat =
            updater model.chatSettings
    in
    ( { model
        | chatSettings = chat
      }
    , chat
    )


handleGetChatResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetChatResponse _ value model =
    case JD.decodeValue (ElmChat.settingsDecoder ChatUpdate) value of
        Err _ ->
            { model
                | error = Just <| "Couldn't decode chat."
            }
                |> withNoCmd

        Ok settings ->
            let
                ( model2, chat2 ) =
                    updateChat
                        model
                        (\chat ->
                            { settings
                                | id = ids.chatOutput
                                , zone = chat.zone
                            }
                        )
            in
            model2
                |> withCmd (ElmChat.restoreScroll chat2)


initialNewReqCmd : Game -> Model -> Cmd Msg
initialNewReqCmd game model =
    send game.isLocal game.interface <|
        let
            gameState =
                game.gameState

            req =
                initialNewReqBody gameState.maxPlayers gameState.winningPoints model.tick
        in
        NewReq
            { req
                | restoreState =
                    Just game.gameState
            }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { page = model.page
    , gameid = model.gameid
    , settings = model.settings
    , styleType = model.styleType
    , notificationsEnabled = model.notificationsEnabled
    , soundEnabled = model.soundEnabled
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    let
        game =
            model.game
    in
    { model
        | page = savedModel.page
        , gameid = savedModel.gameid
        , settings = savedModel.settings
        , styleType = savedModel.styleType
        , notificationsEnabled = savedModel.notificationsEnabled
        , soundEnabled = savedModel.soundEnabled
    }


playerName : Player -> Game -> Maybe String
playerName player game =
    -- TODO: Use UI.playerName
    Dict.get player game.gameState.players


incomingMessage : GameInterface -> Message -> Model -> ( Model, Cmd Msg )
incomingMessage interface message mdl =
    let
        messageLog =
            Debug.log "incomingMessage" <| ED.messageToLogMessage message

        model =
            { mdl | reallyClearStorage = False }

        ( maybeGame, ( model2, cmd2 ) ) =
            case Types.messageToGameid message of
                Nothing ->
                    incomingMessageInternal interface Nothing message model

                Just gameid ->
                    let
                        maybeGame2 =
                            if gameid /= model.gameid then
                                Nothing

                            else
                                let
                                    game =
                                        model.game
                                in
                                Just { game | interface = interface }
                    in
                    incomingMessageInternal interface maybeGame2 message model
    in
    case maybeGame of
        Nothing ->
            model2 |> withCmd cmd2

        Just game ->
            { model2 | game = game }
                |> withCmds [ cmd2, putGame game ]


nextPlayer : Game -> Game
nextPlayer game =
    if not game.isLocal then
        game

    else
        let
            np =
                game.player + 1

            player =
                if np >= game.gameState.maxPlayers then
                    0

                else
                    np

            playerid =
                case DE.find (\_ p -> p == player) game.playerIds of
                    Just ( id, _ ) ->
                        id

                    Nothing ->
                        ""
        in
        { game
            | player = player
            , playerid = playerid
        }


{-| Do the work for `incomingMessage`.

If `maybeGame` is not not `Nothing`, then a game with its `gameid` was found.
If the `Maybe Game` in the result is not `Nothing`, then the `Game` was changed,
and needs to be persisted by `incomingMessage`. Otherwise, it was NOT changed.

-}
incomingMessageInternal : GameInterface -> Maybe Game -> Message -> Model -> ( Maybe Game, ( Model, Cmd Msg ) )
incomingMessageInternal interface maybeGame message model =
    let
        withRequiredGame : GameId -> (Game -> ( Maybe Game, ( Model, Cmd Msg ) )) -> ( Maybe Game, ( Model, Cmd Msg ) )
        withRequiredGame gameid thunk =
            case maybeGame of
                Just game ->
                    thunk game

                Nothing ->
                    ( Nothing
                    , { model
                        | error =
                            Just <| "Bug: there is no session for id: " ++ gameid
                      }
                        |> withNoCmd
                    )
    in
    case message of
        NewRsp { gameid, playerid, player, name, gameState, wasRestored } ->
            let
                game =
                    model.game

                ( model2, chatCmd ) =
                    clearChatSettings True model

                game2 =
                    { game
                        | gameid = gameid
                        , gameState = gameState
                        , player = player
                        , playerid = playerid
                        , isLive = True
                        , interface = interface
                    }
                        |> nextPlayer

                model3 =
                    { model2
                        | game = game2
                        , gameid = gameid
                    }
            in
            ( Just game2
            , model3
                |> withCmd chatCmd
            )

        JoinRsp { gameid, playerid, gameState } ->
            let
                game =
                    model.game

                newPlayer =
                    Dict.size game.playerIds

                game2 =
                    { game
                        | gameid = gameid
                        , playerIds = Dict.insert playerid newPlayer game.playerIds
                        , gameState = gameState
                        , isLive = True
                        , player = newPlayer
                        , interface = interface
                    }
                        |> nextPlayer

                model2 =
                    { model | game = game2 }

                msg =
                    "The game is on!"
            in
            ( Just game2
            , model2
                |> withCmds
                    [ maybeSendNotification game2 False msg model2
                    ]
            )

        LeaveRsp { gameid, participant, name } ->
            -- TODO
            -- If game.local, need to remove player from game.playerIds
            -- Also, update nextPlayer (and Interface.nextPlayer) to
            -- skip non-existent player numbers in live games.
            let
                body : Game -> ( Maybe Game, ( Model, Cmd Msg ) )
                body game =
                    let
                        player =
                            participant

                        leftMsg =
                            name ++ " left" ++ "."

                        game2 =
                            { game
                                | gameid = ""
                                , playerid = ""
                            }

                        model2 =
                            { model
                                | error =
                                    if player == game2.player then
                                        Nothing

                                    else
                                        Just leftMsg
                            }
                    in
                    if game2.isLocal then
                        ( Just game2, model2 |> withNoCmd )

                    else
                        ( Just { game2 | isLive = False }
                        , model2
                            |> withCmds
                                [ if player /= game2.player then
                                    maybeSendNotification game2 True leftMsg model2

                                  else
                                    Cmd.none
                                ]
                        )
            in
            withRequiredGame gameid body

        UpdateRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just { game | gameState = gameState }
                    , model
                        |> withCmd
                            (Task.perform identity <|
                                Task.succeed RestoreSubscriptions
                            )
                    )
                )

        PlayRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    if not game.isLocal then
                        ( Just
                            { game
                                | gameState = gameState
                            }
                        , model
                            |> withCmds
                                [ maybeSendNotification
                                    game
                                    False
                                    "It's your turn in Say Uncle."
                                    model
                                ]
                        )

                    else
                        ( Just ({ game | gameState = gameState } |> nextPlayer)
                        , model |> withNoCmd
                        )
                )

        AnotherGameRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( error, msg ) =
                            if not game.isLocal && not model.requestedNew then
                                let
                                    m =
                                        "Another game."
                                in
                                ( Just m, m )

                            else
                                ( Nothing, "" )

                        mdl2 =
                            { model
                                | requestedNew = False
                                , error = error
                            }

                        cmd =
                            if error == Nothing then
                                Cmd.none

                            else
                                maybeSendNotification game True msg mdl2
                    in
                    ( Just
                        { game
                            | gameState = gameState
                        }
                    , mdl2 |> withCmd cmd
                    )
                )

        GameOverRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just
                        { game | gameState = gameState }
                    , model |> withNoCmd
                    )
                )

        PublicGamesRsp { games } ->
            ( Nothing
            , { model | publicGames = games }
                |> withNoCmd
            )

        PublicGamesUpdateRsp { added, removed } ->
            let
                games =
                    List.filter
                        (\pgaps ->
                            not <| List.member pgaps.publicGame.gameid removed
                        )
                        model.publicGames
            in
            ( Nothing
            , { model | publicGames = List.concat [ games, added ] }
                |> withNoCmd
            )

        StatisticsRsp { statistics, startTime, updateTime } ->
            ( Nothing
            , { model
                | statistics = statistics
                , statisticsTimes = ( startTime, updateTime )
              }
                |> withNoCmd
            )

        ErrorRsp { request, text } ->
            let
                errorReturn () =
                    ( Nothing
                    , { model | error = Just text }
                        |> withNoCmd
                    )
            in
            case WSFED.decodeMessage ED.messageDecoder request of
                Ok (UpdateReq { playerid }) ->
                    -- Server has forgotten the game.
                    -- Restore it.
                    if model.game.playerid == "" then
                        ( Nothing
                        , { model
                            | error =
                                Just "Bug: Can't restore session."
                          }
                            |> withNoCmd
                        )

                    else
                        ( Nothing
                        , webSocketConnect
                            model.game
                            (ConnectionSpec <|
                                RestoreGameConnection model.game
                            )
                            model
                        )

                Ok (NewReq { maybeGameid }) ->
                    case maybeGameid of
                        Nothing ->
                            errorReturn ()

                        Just gameid ->
                            if gameid /= model.gameid then
                                errorReturn ()

                            else
                                let
                                    restoredGame =
                                        model.game
                                in
                                case playerName restoredGame.player restoredGame of
                                    Nothing ->
                                        errorReturn ()

                                    _ ->
                                        ( Nothing
                                        , webSocketConnect
                                            restoredGame
                                            (ConnectionSpec <|
                                                JoinRestoredGameConnection gameid
                                            )
                                            model
                                        )

                Ok (LeaveReq { playerid }) ->
                    let
                        game =
                            model.game
                    in
                    ( Just
                        { game
                            | gameid = ""
                            , playerid = ""
                            , isLive = False
                        }
                    , model |> withNoCmd
                    )

                _ ->
                    errorReturn ()

        ChatRsp { gameid, name, text } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( chat, cmd ) =
                            ElmChat.addLineSpec model.chatSettings <|
                                ElmChat.makeLineSpec text
                                    (Just name)
                                    (Just model.tick)
                    in
                    ( Nothing
                    , { model
                        | chatSettings = chat
                      }
                        |> withCmds
                            [ cmd

                            -- Kluge. ElmChat is supposed to do this
                            , Task.attempt (\_ -> Noop) <|
                                Dom.setViewportOf ids.chatOutput 0 1000000
                            , maybeSendNotification game
                                True
                                ("You got a SayUncle chat message from " ++ name)
                                model
                            ]
                    )
                )

        _ ->
            ( Nothing, model |> withNoCmd )


getScore : Player -> GameState -> Int
getScore player gameState =
    Maybe.withDefault 0 <| Dict.get player gameState.score.points


setPage : Page -> Cmd Msg
setPage page =
    Task.perform SetPage <| Task.succeed page


notificationHandler : Notification.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
notificationHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        Notification.NoResponse ->
            model |> withNoCmd

        Notification.AvailableResponse available ->
            { model | notificationAvailable = Just available }
                |> withNoCmd

        Notification.PermissionResponse permission ->
            let
                enabled =
                    case model.notificationPermission of
                        Nothing ->
                            permission == PermissionGranted

                        _ ->
                            model.notificationsEnabled
            in
            { model
                | notificationPermission =
                    Just permission
                , error =
                    if permission == PermissionDenied then
                        Just "You denied notification permission. This can only be changed in your brower's settings."

                    else
                        model.error
                , notificationsEnabled = enabled
            }
                |> withCmd
                    (if enabled && not model.notificationsEnabled then
                        sendNotification "Notifications Enabled!"

                     else
                        Cmd.none
                    )

        Notification.NotificationResponse notification ->
            let
                n =
                    Debug.log "notification" notification
            in
            model |> withNoCmd

        Notification.ClickResponse id ->
            model
                |> withCmds
                    [ setPage MainPage
                    , notificationCmd (Notification.dismissNotification id)
                    ]

        Notification.ErrorResponse s ->
            { model | error = Just <| "Notification error: " ++ s }
                |> withNoCmd


{-| TODO: properly notify crowd members on unshown session change.
-}
maybeSendNotification : Game -> Bool -> String -> Model -> Cmd Msg
maybeSendNotification game ignoreWhoseTurn title model =
    if
        model.notificationsEnabled
            && (ignoreWhoseTurn || game.gameState.whoseTurn == game.player)
            && not game.isLocal
            && not model.visible
    then
        sendNotification title

    else
        Cmd.none


sendNotification : String -> Cmd Msg
sendNotification title =
    Notification.displayNotification title
        |> notificationCmd


notificationCmd : Notification.Message -> Cmd Msg
notificationCmd message =
    message
        |> Notification.send (getCmdPort Notification.moduleName ())


socketHandler : Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | error = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            let
                string =
                    received.message
            in
            case WSFED.decodeMessage ED.messageDecoder string of
                Err errmsg ->
                    { model | error = Just errmsg }
                        |> withNoCmd

                Ok message ->
                    let
                        game =
                            model.game
                    in
                    if game.isLocal then
                        { model
                            | error =
                                Just "Got a WebSocket message for a local game."
                        }
                            |> withNoCmd

                    else
                        { model | error = Nothing }
                            |> withCmd
                                (Task.perform (IncomingMessage False game.interface) <|
                                    Task.succeed message
                                )

        ClosedResponse { expected, reason } ->
            let
                game =
                    model.game

                game2 =
                    if game.isLocal then
                        game

                    else
                        { game | isLive = False }
            in
            { model
                | game = game
                , connectionSpecQueue = Fifo.empty
                , error =
                    if Debug.log "ClosedResponse, expected" expected then
                        model.error

                    else
                        Just <| "Connection unexpectedly closed: " ++ reason
            }
                |> withNoCmd

        ConnectedResponse crrec ->
            connectedResponse model

        _ ->
            model |> withNoCmd


connectedResponse : Model -> ( Model, Cmd Msg )
connectedResponse model =
    let
        ( maybeConnectionSpec, model2 ) =
            removeConnectionSpec model
    in
    { model2 | error = Nothing }
        |> withCmds
            [ if isConnectionSpecQueueEmpty model2 then
                Cmd.none

              else
                Task.perform identity <|
                    Task.succeed DoConnectedResponse
            , case maybeConnectionSpec of
                Nothing ->
                    Cmd.none

                Just { connectionReason } ->
                    processConnectionReason model.game connectionReason model2
            ]


processConnectionReason : Game -> ConnectionReason -> Model -> Cmd Msg
processConnectionReason game connectionReason model =
    -- Get here by pressing the "Start Session" button.
    let
        interface =
            game.interface

        isLocal =
            game.isLocal
    in
    case Debug.log "processConnectionReason" connectionReason of
        StartGameConnection ->
            let
                settings =
                    model.settings
            in
            send isLocal interface <|
                NewReq
                    { name = settings.name
                    , publicType =
                        if not settings.isPublic then
                            NotPublic

                        else
                            case settings.forName of
                                "" ->
                                    EntirelyPublic

                                forName ->
                                    PublicFor forName
                    , maxPlayers = settings.maxPlayers
                    , winningPoints = settings.winningPoints
                    , seedInt = Time.posixToMillis model.tick
                    , restoreState = Nothing
                    , maybeGameid = Nothing
                    }

        JoinGameConnection gameid inCrowd ->
            send isLocal interface <|
                let
                    i =
                        debugInterface interface
                in
                JoinReq
                    { gameid = gameid
                    , name = model.settings.name
                    }

        PublicGamesConnection ->
            send isLocal interface <|
                PublicGamesReq
                    { subscribe = model.page == PublicPage
                    , forName = model.settings.name
                    , gameid = Just model.game.gameid
                    }

        StatisticsConnection ->
            send isLocal interface <|
                StatisticsReq
                    { subscribe = model.page == StatisticsPage
                    }

        UpdateConnection playerid ->
            send isLocal interface <|
                UpdateReq
                    { playerid = playerid }

        RestoreGameConnection localGame ->
            let
                player =
                    localGame.player
            in
            case playerName player localGame of
                Nothing ->
                    Cmd.none

                Just name ->
                    let
                        gameState =
                            localGame.gameState
                    in
                    send isLocal interface <|
                        NewReq
                            { name = name
                            , maxPlayers = gameState.maxPlayers
                            , winningPoints = gameState.winningPoints
                            , seedInt = 0 --not used
                            , publicType = NotPublic
                            , restoreState = Just gameState
                            , maybeGameid = Just localGame.gameid
                            }

        JoinRestoredGameConnection gameid ->
            -- Errors are generated in ErrorRsp handler,
            -- before it generates the Cmd that gets here.
            if gameid /= model.game.gameid then
                Cmd.none

            else
                let
                    localGame =
                        model.game
                in
                case playerName localGame.player localGame of
                    Nothing ->
                        Cmd.none

                    Just name ->
                        send isLocal interface <|
                            JoinReq
                                { gameid = gameid
                                , name = name
                                }


debugInterface : GameInterface -> GameInterface
debugInterface interface =
    interface


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) (Dom.focus id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearClearStorage =
            model.reallyClearStorage
                && (case msg of
                        ClearStorage ->
                            False

                        Tick _ ->
                            False

                        _ ->
                            True
                   )

        ( mdl, cmd ) =
            updateInternal msg <|
                if clearClearStorage then
                    { model | reallyClearStorage = False, error = Nothing }

                else
                    model

        players =
            mdl.game.gameState.players

        focus =
            --not game.isLocal && game.isLive && white /= "" && black /= ""
            --Might be able to be smart and do this just on desktop, but not for now.
            --Focusing on mobile zooms the screen and shows the keyboard.
            False

        doSave =
            case msg of
                Noop ->
                    False

                Tick _ ->
                    model.tick == zeroTick

                Click _ ->
                    cmd == Cmd.none

                NewGame ->
                    --False
                    True

                Process _ ->
                    False

                IncomingMessage _ _ _ ->
                    -- cmd == Cmd.none
                    True

                Reload ->
                    False

                MaybeClearStorage ->
                    False

                ClearStorage ->
                    False

                ChatUpdate _ _ ->
                    False

                ChatSend _ _ ->
                    False

                ChatClear ->
                    False

                PlaySound _ ->
                    False

                DelayedAction _ _ ->
                    False

                _ ->
                    True
    in
    mdl
        |> withCmds
            [ cmd
            , if focus && doSave then
                focusId ids.chatInput

              else
                Cmd.none
            , if model.started && doSave then
                putModel mdl

              else
                Cmd.none
            ]


messageQueueLength : Int
messageQueueLength =
    20


fifoLength : Fifo a -> Int
fifoLength fifo =
    List.length <| Fifo.toList fifo


recordMessage : Bool -> Bool -> Message -> Model -> Model
recordMessage interfaceIsLocal isSend message model =
    let
        queue =
            if fifoLength model.messageQueue >= messageQueueLength then
                let
                    ( _, q ) =
                        Fifo.remove model.messageQueue
                in
                q

            else
                model.messageQueue
    in
    { model
        | messageQueue =
            Fifo.insert (MessageQueueEntry interfaceIsLocal isSend message) queue
    }


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        game =
            model.game

        gameState =
            game.gameState

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        Tick posix ->
            { model
                | tick = posix
                , game =
                    if model.tick /= zeroTick then
                        if not game.isLocal then
                            game

                        else
                            { game
                                | interface =
                                    updateServerTime model.tick game.interface
                            }

                    else
                        { game
                            | interface =
                                updateServerSeed (Just <| makeSeed posix)
                                    game.interface
                        }
            }
                |> withCmd
                    (if model.tick == zeroTick && model.started then
                        -- This is also done by storageHandler,
                        -- but only if the seed has been
                        -- initialized.
                        get pk.model

                     else
                        Cmd.none
                    )

        IncomingMessage interfaceIsLocal interface message ->
            incomingMessage interface message <|
                recordMessage interfaceIsLocal False message model

        RecordMessage interfaceIsLocal isSend message ->
            recordMessage interfaceIsLocal isSend message model
                |> withNoCmd

        SetIsLocal isLocal ->
            if game.isLocal == isLocal then
                model |> withNoCmd

            else
                let
                    interface =
                        if isLocal then
                            proxyServer <| modelSeed model

                        else
                            game.interface

                    game2 =
                        { game
                            | isLocal = isLocal
                            , isLive = False
                            , playerid = ""
                            , interface = interface
                            , interfaceIsProxy = isLocal
                        }

                    model2 =
                        { model
                            | game = game2
                            , gameid = ""
                        }
                in
                model2
                    |> withCmds
                        [ putGame game2
                        , if isLocal && not game.isLocal then
                            Cmd.batch
                                [ if game.isLive then
                                    send game.isLocal interface <|
                                        LeaveReq { playerid = game.playerid }

                                  else
                                    Cmd.none
                                , initialNewReqCmd game2 model
                                ]

                          else
                            Cmd.none
                        ]

        SetDarkMode darkMode ->
            let
                styleType =
                    if darkMode then
                        DarkStyle

                    else
                        LightStyle
            in
            { model | styleType = styleType }
                |> withNoCmd

        SetName name ->
            { model | settings = { settings | name = name } }
                |> withNoCmd

        SetMaxPlayersString maxPlayersString ->
            let
                maxPlayers =
                    case String.toInt maxPlayersString of
                        Nothing ->
                            settings.maxPlayers

                        Just mp ->
                            mp
            in
            { model
                | settings =
                    { settings
                        | maxPlayersString = maxPlayersString
                        , maxPlayers = maxPlayers
                    }
            }
                |> withNoCmd

        SetWinningPointsString winningPointsString ->
            let
                winningPoints =
                    case String.toInt winningPointsString of
                        Nothing ->
                            settings.winningPoints

                        Just wp ->
                            wp
            in
            { model
                | settings =
                    { settings
                        | winningPointsString = winningPointsString
                        , winningPoints = winningPoints
                    }
            }
                |> withNoCmd

        SetIsPublic isPublic ->
            { model | settings = { settings | isPublic = isPublic } }
                |> withCmd
                    (if isPublic && not settings.isPublic then
                        focusId ids.forName

                     else
                        Cmd.none
                    )

        SetForName forName ->
            { model | settings = { settings | forName = forName } }
                |> withNoCmd

        SetServerUrl serverUrl ->
            { model | game = { game | serverUrl = serverUrl } }
                |> withNoCmd

        SetGameid gameid ->
            { model | gameid = gameid }
                |> withNoCmd

        SetPage page ->
            let
                mdl =
                    { model | page = page }

                ( mdl2, cmd ) =
                    if page == PublicPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec PublicGamesConnection)
                            mdl

                    else if page == StatisticsPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec StatisticsConnection)
                            mdl

                    else
                        ( mdl, Cmd.none )

                interface =
                    mdl2.game.interface

                cmd2 =
                    if page == StatisticsPage then
                        send game.isLocal interface <|
                            StatisticsReq { subscribe = True }

                    else if model.page == StatisticsPage then
                        send game.isLocal interface <|
                            StatisticsReq { subscribe = False }

                    else if page == MainPage then
                        let
                            chat =
                                model.chatSettings
                        in
                        ElmChat.restoreScroll chat

                    else
                        Cmd.none

                cmd3 =
                    if page == PublicPage then
                        send game.isLocal interface <|
                            PublicGamesReq
                                { subscribe = True
                                , forName = ""
                                , gameid = Nothing
                                }

                    else if model.page == PublicPage then
                        send game.isLocal interface <|
                            PublicGamesReq
                                { subscribe = False
                                , forName = ""
                                , gameid = Nothing
                                }

                    else
                        Cmd.none
            in
            mdl2 |> withCmds [ cmd, cmd2, cmd3 ]

        SetHideTitle hideTitle ->
            { model | settings = { settings | hideTitle = hideTitle } }
                |> withNoCmd

        NewGame ->
            if game.isLive then
                { model
                    | error = Just "Cannot create a new game while playing."
                }
                    |> withNoCmd

            else if game.player /= 0 && not game.isLocal then
                { model
                    | error = Just "Only the game creator can start a game."
                }
                    |> withNoCmd

            else
                { model | requestedNew = True }
                    |> withCmd
                        (send model.game.isLocal model.game.interface <|
                            PlayReq
                                { playerid = game.playerid
                                , placement = ChooseNew
                                }
                        )

        StartGame ->
            startGame model

        Join ->
            join model False

        JoinGame gameid ->
            join { model | gameid = gameid } False

        Disconnect ->
            disconnect model

        SetNotificationsEnabled enabled ->
            if not enabled then
                { model | notificationsEnabled = False }
                    |> withNoCmd

            else
                case model.notificationPermission of
                    Nothing ->
                        model
                            |> withCmd
                                (notificationCmd Notification.requestPermission)

                    Just PermissionDenied ->
                        { model | notificationsEnabled = False }
                            |> withNoCmd

                    _ ->
                        { model | notificationsEnabled = enabled }
                            |> withCmd
                                (if enabled then
                                    sendNotification "Notifications Enabled!"

                                 else
                                    Cmd.none
                                )

        SetShowMessageQueue showMessageQueue ->
            { model | showMessageQueue = showMessageQueue }
                |> withNoCmd

        SetSoundEnabled bool ->
            { model | soundEnabled = bool }
                |> withNoCmd

        MakeInitialBoard ->
            let
                game2 =
                    { game
                        | gameState =
                            { gameState
                                | board =
                                    Board.initial
                                        2
                                        (Random.initialSeed <|
                                            Time.posixToMillis model.tick
                                        )
                                , whoseTurn = 0
                            }
                    }
            in
            { model | game = game2 }
                |> withCmd (putGame game2)

        Reload ->
            model |> withCmd Navigation.reloadAndSkipCache

        MaybeClearStorage ->
            { model
                | reallyClearStorage = True
                , error = Just "Click the \"Clear Storage Now!\" button to clear all storage."
            }
                |> withNoCmd

        ClearStorage ->
            let
                ( mdl, cmd ) =
                    init JE.null "url" model.key
            in
            { mdl
                | started = True
                , windowSize = model.windowSize
                , notificationAvailable = model.notificationAvailable
                , tick = model.tick
            }
                |> withCmds [ clearStorage ]

        Click boardClick ->
            if gameState.winner /= NoWinner || (not <| isPlaying model) then
                model |> withNoCmd

            else
                doClick boardClick model

        ChatUpdate chatSettings cmd ->
            updateChat model (always chatSettings)
                |> Tuple.first
                |> withCmds [ cmd, putChat chatSettings ]

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            clearChatSettings False model

        PlaySound file ->
            if not model.soundEnabled then
                model |> withNoCmd

            else
                { model | soundFile = Just file }
                    |> withCmd (playAudio file)

        DelayedAction updater time ->
            updater { model | delayTime = time }

        SetZone zone ->
            let
                chat =
                    model.chatSettings
            in
            { model
                | zone = zone
                , chatSettings = { chat | zone = zone }
            }
                |> withNoCmd

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        VisibilityChange visibility ->
            { model | visible = visibility }
                |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd

        DoConnectedResponse ->
            connectedResponse model

        RestoreSubscriptions ->
            maybeRestoreSubscriptions model

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res


clearChatSettings : Bool -> Model -> ( Model, Cmd Msg )
clearChatSettings clearInput model =
    let
        ( model2, chat ) =
            updateChat
                model
                (\chat2 ->
                    { chat2
                        | lines = []
                        , input =
                            if clearInput then
                                ""

                            else
                                chat2.input
                    }
                )
    in
    model2 |> withCmd (putChat chat)


chatSend : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSend line chatSettings model =
    model
        |> withCmd (delayedAction <| chatSendInternal line chatSettings)


chatSendInternal : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSendInternal line chatSettings model =
    let
        ( model2, _ ) =
            updateChat model (always chatSettings)
    in
    model2
        |> withCmd
            (send model2.game.isLocal model2.game.interface <|
                ChatReq
                    { playerid = model.game.playerid
                    , text = line
                    }
            )


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


makeWebSocketServer : Model -> GameInterface
makeWebSocketServer model =
    WebSocketFramework.makeServer
        (getCmdPort WebSocket.moduleName ())
        ED.messageEncoder
        model.game.serverUrl
        Noop


webSocketConnect : Game -> ConnectionSpec -> Model -> ( Model, Cmd Msg )
webSocketConnect game spec model =
    if game.isLocal then
        let
            newGame =
                { game
                    | interface =
                        if game.interfaceIsProxy then
                            game.interface

                        else
                            proxyServer <| modelSeed model
                    , interfaceIsProxy = True
                    , isLive = True
                }

            mdl =
                { model | game = newGame }
        in
        mdl
            |> withCmd
                (processConnectionReason newGame spec.connectionReason mdl)

    else
        let
            newGame =
                { game
                    | interface =
                        if True then
                            --game.interfaceIsProxy then
                            makeWebSocketServer model

                        else
                            game.interface
                    , interfaceIsProxy = False
                }
        in
        { model | game = newGame }
            |> insertConnectionSpec (Debug.log "webSocketConnect" spec)
            |> withCmd
                (WebSocket.makeOpen game.serverUrl
                    |> webSocketSend
                )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    let
        settings =
            model.settings
    in
    case String.toInt settings.maxPlayersString of
        Nothing ->
            { model
                | error = Just "Max Players must be an integer"
            }
                |> withNoCmd

        Just maxPlayers ->
            webSocketConnect model.game
                (ConnectionSpec StartGameConnection)
                { model
                    | settings = { settings | maxPlayers = maxPlayers }
                }


join : Model -> Bool -> ( Model, Cmd Msg )
join model inCrowd =
    let
        gameid =
            model.gameid

        game =
            model.game

        model2 =
            { model
                | game =
                    -- Needed by JoinRsp
                    { game | gameid = gameid }
            }

        ( model3, cmd3 ) =
            webSocketConnect model2.game
                (ConnectionSpec <|
                    JoinGameConnection gameid inCrowd
                )
                model2
    in
    model3 |> withCmds [ cmd3, setPage MainPage ]


disconnect : Model -> ( Model, Cmd Msg )
disconnect model =
    let
        game =
            model.game

        gameState =
            if not game.isLocal then
                game.gameState

            else
                let
                    gs =
                        game.gameState
                in
                { gs
                    | board =
                        { tableau = Array.empty
                        , stock = Deck.newDeck []
                        , turnedStock = Nothing
                        , hands = Array.empty
                        , seed = gs.board.seed
                        }
                    , score = Types.zeroScore
                    , players = Dict.empty
                    , player = 0
                    , whoseTurn = 0
                    , state = InitialState
                    , winner = NoWinner
                    , matchWinner = Nothing
                }

        game2 =
            if not game.isLocal then
                { game
                    | isLive = False
                }

            else
                { game | gameState = gameState }
    in
    { model | game = game2 }
        |> withCmds
            [ putGame game2
            , if game.isLive then
                send model.game.isLocal model.game.interface <|
                    LeaveReq { playerid = game.playerid }

              else
                Cmd.none
            ]


send : Bool -> GameInterface -> Message -> Cmd Msg
send interfaceIsLocal interface message =
    let
        logMessage =
            Debug.log "send" <| ED.messageToLogMessage message
    in
    Cmd.batch
        [ ServerInterface.send interface message
        , Task.perform (RecordMessage interfaceIsLocal True) <| Task.succeed message
        ]


doClick : BoardClick -> Model -> ( Model, Cmd Msg )
doClick boardClick model =
    -- TODO
    model |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , onVisibilityChange VisibilityChange -- Browser.Events.onVisibilityChange doesn't work
        , PortFunnels.subscriptions Process model
        , Time.every 900 Tick
        ]



---
--- Persistence
---


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel
    in
    put pk.model <| Just value


putChat : ChatSettings -> Cmd Msg
putChat settings =
    (Just <| ElmChat.settingsEncoder settings)
        |> put pk.chat


getChat : Cmd Msg
getChat =
    getLabeled pk.chat pk.chat


putGame : Game -> Cmd Msg
putGame game =
    (Just <| ED.encodeGame game)
        |> put pk.game


getGame : Cmd Msg
getGame =
    getLabeled pk.game pk.game


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend <| Debug.log "LocalStorage" (LocalStorage.get key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (Debug.log "LocalStorage" <|
            LocalStorage.getLabeled label key
        )


listKeys : String -> Cmd Msg
listKeys prefix =
    localStorageSend (LocalStorage.listKeys prefix)


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


clearStorage : Cmd Msg
clearStorage =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "SayUncle"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        , NotificationHandler notificationHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , chat = "chat"
    , game = "game"
    }
