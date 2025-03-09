port module Zephyrnot.Server.Server exposing (main)

import Set exposing (Set)
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , Socket
        , UserFunctions
        , program
        , verbose
        )
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , GameId
        , InputPort
        , OutputPort
        )
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Interface as Interface
import Zephyrnot.Types as Types
    exposing
        ( Decoration(..)
        , GameState
        , Message(..)
        , Player
        , PlayerNames
        , PublicType(..)
        , SubscriptionSet
        )


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Player


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


tos : Int -> String
tos x =
    String.fromInt x


errorWrapper : Error Message -> Message
errorWrapper { kind, description, message } =
    case kind of
        JsonParseError ->
            let
                err =
                    case message of
                        Err msg ->
                            msg

                        Ok msg ->
                            Debug.toString msg
            in
            ErrorRsp
                { request = description
                , text = "JSON parser error: " ++ err
                }

        _ ->
            ErrorRsp
                { request = ""
                , text = Debug.toString message
                }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoder
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorWrapper
    }


{-| Two weeks
-}
deathRowDuration : Int
deathRowDuration =
    14 * 24 * 60 * 60 * 1000


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender mdl socket state request response =
    let
        model =
            if WebSocketFramework.Server.getDeathRowDuration mdl == deathRowDuration then
                mdl

            else
                WebSocketFramework.Server.setDeathRowDuration mdl deathRowDuration

        ( state2, cmd ) =
            case request of
                PublicGamesReq { subscribe, forName } ->
                    ( handlePublicGamesSubscription subscribe forName socket state
                    , Cmd.none
                    )

                _ ->
                    case response of
                        LeaveRsp { gameid } ->
                            let
                                ( model2, cmd2 ) =
                                    gamesDeleter model [ gameid ] state
                            in
                            ( WebSocketFramework.Server.getState model2, cmd2 )

                        _ ->
                            ( state, Cmd.none )

        sender =
            case request of
                UpdateReq _ ->
                    sendToOne

                PublicGamesReq { subscribe, forName } ->
                    sendToOne

                _ ->
                    case response of
                        NewRsp { gameid } ->
                            let
                                sockets =
                                    WebSocketFramework.Server.otherSockets gameid
                                        ""
                                        model
                            in
                            sendNewRsp model state

                        JoinRsp { gameid } ->
                            let
                                sockets =
                                    WebSocketFramework.Server.otherSockets
                                        gameid
                                        ""
                                        model
                            in
                            sendJoinRsp model state

                        PlayRsp _ ->
                            sendPlayRsp model

                        AnotherGameRsp record ->
                            \_ _ ->
                                Cmd.batch
                                    [ sendToOne response socket
                                    , sendToOthers model
                                        (AnotherGameRsp
                                            { record
                                                | player =
                                                    Types.otherPlayer record.player
                                            }
                                        )
                                        socket
                                    ]

                        _ ->
                            sendToAll model
    in
    ( WebSocketFramework.Server.setState model state2
    , Cmd.batch [ cmd, sender response socket ]
    )


handlePublicGamesSubscription : Bool -> String -> Socket -> ServerState -> ServerState
handlePublicGamesSubscription subscribe forName socket state =
    let
        gs =
            case state.state of
                Nothing ->
                    Interface.emptyGameState <| PlayerNames "" ""

                Just gameState ->
                    gameState

        private =
            gs.private

        subscribers =
            Set.filter (\( sock, _ ) -> socket /= sock) private.subscribers
    in
    { state
        | state =
            Just
                { gs
                    | private =
                        { private
                            | subscribers =
                                if subscribe then
                                    Set.insert ( socket, forName ) subscribers

                                else
                                    subscribers
                        }
                }
    }


sendToOne : Message -> Socket -> Cmd Msg
sendToOne response socket =
    WebSocketFramework.Server.sendToOne ED.messageEncoder response outputPort socket


sendToAll : Model -> Message -> Socket -> Cmd Msg
sendToAll model response socket =
    case Types.messageToGameid response of
        Nothing ->
            sendToOne response socket

        Just gameid ->
            WebSocketFramework.Server.sendToAll gameid
                model
                ED.messageEncoder
                response


sendToOthers : Model -> Message -> Socket -> Cmd Msg
sendToOthers model response socket =
    case Types.messageToGameid response of
        Nothing ->
            Cmd.none

        Just gameid ->
            WebSocketFramework.Server.sendToOthers gameid
                socket
                model
                ED.messageEncoder
                response


sendNewRsp : Model -> ServerState -> Message -> Socket -> Cmd Msg
sendNewRsp model state response socket =
    -- Need to send new public game to subscribers.
    let
        notifications =
            case state.state of
                Nothing ->
                    []

                Just gs ->
                    case response of
                        NewRsp { gameid, player, name, publicType } ->
                            if publicType == NotPublic then
                                []

                            else
                                let
                                    publicGame =
                                        { gameid = gameid
                                        , creator = name
                                        , player = player
                                        , forName =
                                            case publicType of
                                                PublicFor for ->
                                                    Just for

                                                _ ->
                                                    Nothing
                                        }

                                    notification =
                                        sendToOne
                                            (PublicGamesUpdateRsp
                                                { added = [ publicGame ]
                                                , removed = []
                                                }
                                            )
                                in
                                gs.private.subscribers
                                    |> Set.toList
                                    |> List.filterMap
                                        (\( sock, forName ) ->
                                            case publicType of
                                                EntirelyPublic ->
                                                    Just <| notification sock

                                                PublicFor for ->
                                                    if forName == for then
                                                        Just <| notification sock

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )

                        _ ->
                            []
    in
    Cmd.batch <|
        sendToOne response socket
            :: notifications


removedGameNotifications : GameId -> ServerState -> Cmd Msg
removedGameNotifications gameid state =
    case state.state of
        Nothing ->
            Cmd.none

        Just gs ->
            let
                notification =
                    sendToOne <|
                        PublicGamesUpdateRsp
                            { added = []
                            , removed = [ gameid ]
                            }
            in
            gs.private.subscribers
                |> Set.toList
                |> List.map
                    (\( sock, _ ) ->
                        notification sock
                    )
                |> Cmd.batch


sendJoinRsp : Model -> ServerState -> Message -> Socket -> Cmd Msg
sendJoinRsp model state response socket =
    let
        notifications =
            case response of
                JoinRsp { gameid } ->
                    removedGameNotifications gameid state

                _ ->
                    Cmd.none
    in
    [ notifications
    , case response of
        JoinRsp record ->
            [ sendToOne response socket
            , sendToOthers model
                (JoinRsp { record | playerid = Nothing })
                socket
            ]
                |> Cmd.batch

        _ ->
            sendToAll model response socket
    ]
        |> Cmd.batch


sendPlayRsp : Model -> Message -> Socket -> Cmd Msg
sendPlayRsp model response socket =
    case response of
        PlayRsp playRecord ->
            let
                ( toOne, tagger ) =
                    case playRecord.decoration of
                        RowSelectedDecoration _ ->
                            ( True, RowSelectedDecoration )

                        ColSelectedDecoration _ ->
                            ( True, ColSelectedDecoration )

                        _ ->
                            ( False, \_ -> NoDecoration )
            in
            if toOne then
                Cmd.batch
                    [ sendToOne response socket
                    , sendToOthers model
                        (PlayRsp { playRecord | decoration = tagger -1 })
                        socket
                    ]

            else
                sendToAll model response socket

        _ ->
            sendToAll model response socket


gamesDeleter : Model -> List GameId -> ServerState -> ( Model, Cmd Msg )
gamesDeleter model gameids state =
    case state.state of
        Nothing ->
            ( model, Cmd.none )

        Just gs ->
            let
                private =
                    gs.private

                subscribers =
                    private.subscribers

                loop : GameId -> ( SubscriptionSet, Cmd Msg ) -> ( SubscriptionSet, Cmd Msg )
                loop gameid ( subscribers2, notifications ) =
                    let
                        inner : Socket -> ( SubscriptionSet, Cmd Msg ) -> ( SubscriptionSet, Cmd Msg )
                        inner socket ( subscribers3, notifications2 ) =
                            ( Set.filter (\( sock, _ ) -> sock /= socket)
                                subscribers3
                            , [ removedGameNotifications gameid state
                              , notifications2
                              ]
                                |> Cmd.batch
                            )

                        sockets =
                            WebSocketFramework.Server.otherSockets gameid
                                ""
                                model
                    in
                    List.foldl inner ( subscribers2, notifications ) sockets

                ( subscribers4, notifications3 ) =
                    List.foldl loop ( subscribers, Cmd.none ) gameids

                state2 =
                    { state
                        | state =
                            Just
                                { gs
                                    | private =
                                        { private
                                            | subscribers = subscribers4
                                        }
                                }
                    }
            in
            ( WebSocketFramework.Server.setState model state2
            , notifications3
            )


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = Interface.messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just Types.messageToGameid
    , messageToPlayerid = Just Types.messageToPlayerid
    , autoDeleteGame = Just (\gameid serverState -> False)
    , gamesDeleter = Just gamesDeleter
    , playersDeleter = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


{-| Debugging version
-}
messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    Interface.messageProcessor (Debug.log "messageProcessor" state)
        (Debug.log "  message" message)
        |> Debug.log "  output"


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
