---------------------------------------------------------------------
--
-- Client.elm
-- Simple low-level client for WebSocket server
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Client exposing (Model, Msg(..), br, init, is13, main, messageView, onEnter, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Task
import Url
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.Types exposing (GameId, PlayerId)
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Interface as Interface
import Zephyrnot.Types as Types
    exposing
        ( Choice(..)
        , GameState
        , Message(..)
        , Player(..)
        , PlayerNames
        , PublicType(..)
        , Winner(..)
        )


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List String
    , gameState : GameState
    , gameid : String
    , zephyrus : String
    , notus : String
    , input : String
    , server : String
    , error : Maybe String
    , state : State
    }


init : String -> ( Model, Cmd Msg )
init server =
    ( { messages = []
      , gameState = Interface.emptyGameState (PlayerNames "" "")
      , gameid = ""
      , zephyrus = ""
      , notus = ""
      , input = ""
      , server = server
      , error = Nothing
      , state = PortFunnels.initialState "unusedLocalStoragePrefix"
      }
    , openSocket server
    )


openSocket : String -> Cmd Msg
openSocket server =
    WebSocket.makeOpen
        (Debug.log "openSocket" server)
        |> WebSocket.send cmdPort



-- UPDATE


type Msg
    = Noop
    | InputMessage String
    | SubmitMessage
    | ClickMessage String
    | ServerMessage String
    | Process Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InputMessage value ->
            ( { model | input = value }
            , Cmd.none
            )

        SubmitMessage ->
            ( { model
                | input = ""
                , error = Nothing
              }
            , WebSocket.makeSend model.server model.input
                |> WebSocket.send cmdPort
            )

        ClickMessage value ->
            ( { model | input = value }
            , Task.attempt (\_ -> Noop) <| Dom.focus "input"
            )

        ServerMessage message ->
            ( { model
                | messages = message :: model.messages
              }
            , Cmd.none
            )

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )

                Ok res ->
                    res



-- WebSocket interface


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort


{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


{-| The real output port.
-}
cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | state = state }
    in
    case response of
        WebSocket.MessageReceivedResponse received ->
            let
                string =
                    received.message

                mdl2 =
                    { model | messages = string :: model.messages }

                model2 =
                    case WSFED.decodeMessage ED.messageDecoder string of
                        Err errmsg ->
                            { mdl2 | error = Just errmsg }

                        Ok message ->
                            case message of
                                NewRsp { gameid, playerid } ->
                                    { mdl2
                                        | gameid = gameid
                                        , zephyrus = playerid
                                        , notus = ""
                                    }

                                JoinRsp { playerid, gameState } ->
                                    { mdl2
                                        | notus =
                                            case playerid of
                                                Just p ->
                                                    p

                                                Nothing ->
                                                    ""
                                        , gameState = gameState
                                    }

                                ResignRsp { gameState } ->
                                    { mdl2 | gameState = gameState }

                                GameOverRsp { gameState } ->
                                    { mdl2 | gameState = gameState }

                                AnotherGameRsp { gameState } ->
                                    { mdl2 | gameState = gameState }

                                LeaveRsp _ ->
                                    { mdl2
                                        | gameid = ""
                                        , zephyrus = ""
                                        , notus = ""
                                    }

                                _ ->
                                    mdl2
            in
            ( model2, Cmd.none )

        ErrorResponse error ->
            ( { model | error = Just <| WebSocket.errorToString error }
            , case error of
                WebSocket.SocketNotOpenError _ ->
                    openSocket model.server

                _ ->
                    Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process



-- VIEW


onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
        (E.keyCode |> Decode.andThen (is13 message))


is13 : a -> Int -> Decoder a
is13 a code =
    if code == 13 then
        Decode.succeed a

    else
        Decode.fail "not the right key code"


messageView : String -> Html Msg
messageView message =
    H.li
        []
        [ H.text message ]


br : Html Msg
br =
    H.br [] []


newReq : Message
newReq =
    NewReq
        { name = "Bill"
        , player = Zephyrus
        , publicType = NotPublic
        , restoreState = Nothing
        }


chatReq : PlayerId -> Message
chatReq playerid =
    ChatReq
        { playerid = playerid
        , text = "Hello"
        }


validMessages : Model -> List Message
validMessages model =
    let
        { gameState, gameid, zephyrus, notus } =
            model
    in
    if gameid == "" then
        [ newReq ]

    else if notus == "" then
        [ JoinReq
            { gameid = gameid
            , name = "Chris"
            }
        , newReq
        ]

    else if gameState.winner /= NoWinner then
        [ PlayReq
            { playerid = model.zephyrus
            , placement = ChooseNew Zephyrus
            }
        , newReq
        , chatReq zephyrus
        , chatReq notus
        ]

    else
        [ PlayReq
            { playerid = zephyrus
            , placement = ChooseCol 0
            }
        , PlayReq
            { playerid = notus
            , placement = ChooseRow 0
            }
        , PlayReq
            { playerid = zephyrus
            , placement = ChooseResign Zephyrus
            }
        , PlayReq
            { playerid = notus
            , placement = ChooseResign Notus
            }
        , UpdateReq { playerid = zephyrus }
        , UpdateReq { playerid = notus }
        , LeaveReq { playerid = zephyrus }
        , LeaveReq { playerid = notus }
        , chatReq zephyrus
        , chatReq notus
        ]


messageStrings : List Message -> List String
messageStrings messages =
    messages
        |> List.map (WSFED.encodeMessage ED.messageEncoder)


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.input
            [ A.id "input"
            , A.type_ "text"
            , A.placeholder "Message..."
            , A.value model.input
            , A.size 100
            , E.onInput InputMessage
            , onEnter SubmitMessage
            ]
            []
        , H.p []
            [ H.text "Examples:"
            , H.span []
                (validMessages model
                    |> messageStrings
                    |> List.map
                        (\s ->
                            H.span []
                                [ br
                                , H.a
                                    [ A.href "#"
                                    , E.onClick <| ClickMessage s
                                    , A.class "message"
                                    ]
                                    [ H.text s ]
                                ]
                        )
                )
            ]
        , H.ul [] (List.map messageView model.messages)
        ]
