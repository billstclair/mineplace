----------------------------------------------------------------------
--
-- Main.elm
-- Top-level for MinePlace application.
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Debug exposing (log)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , span
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , name
        , placeholder
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Mastodon.PortFunnels as PortFunnels exposing (FunnelDict, Handler(..), State)
import Mastodon.WebSocket exposing (Event(..), StreamType(..))
import MinePlace.Board as Board
    exposing
        ( addPlayer
        , canMove
        , getCell
        , removePlayer
        , setCell
        , simpleBoard
        , updatePlayer
        )
import MinePlace.Entities exposing (copyright, nbsp)
import MinePlace.Persistence as Persistence
    exposing
        ( PersistentThing(..)
        , boardIdKey
        , decodePersistentThing
        , modelKey
        , playerIdKey
        , writeBoard
        , writeModel
        , writePlayer
        )
import MinePlace.Render exposing (render2d, render3d, renderControls)
import MinePlace.Styles as Styles
import MinePlace.Types
    exposing
        ( Board
        , Direction(..)
        , Layout(..)
        , Location
        , Model
        , Msg(..)
        , Operation(..)
        , Player
        , Size
        , Started(..)
        , Write(..)
        , currentBoardId
        , currentPlayerId
        , initialPlayer
        , operationToDirection
        )
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket
import Svg.Button as Button exposing (Button, RepeatTime(..))
import Task
import Time exposing (Posix)
import Url exposing (Url)


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform
        (\{ viewport } ->
            InitialSize <| Size viewport.width viewport.height
        )
        Dom.getViewport


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialButtonSize : Button.Size
initialButtonSize =
    ( 100, 100 )


delay : Float
delay =
    50


normalRepeatTime : RepeatTime
normalRepeatTime =
    RepeatTimeWithInitialDelay (10 * delay) (2 * delay)


initialRepeatingButton : Operation -> Button Operation
initialRepeatingButton operation =
    Button.repeatingButton normalRepeatTime initialButtonSize operation


initialModel : Model
initialModel =
    let
        board =
            addPlayer initialPlayer simpleBoard
    in
    { windowSize = initialSize
    , board = { board | id = currentBoardId }
    , player = initialPlayer
    , layout = NormalLayout
    , isTouchAware = False
    , delayLeft = 0
    , forwardButton = initialRepeatingButton GoForward
    , backButton = initialRepeatingButton GoBack
    , subscription = Nothing
    , started = NotStarted
    , funnelState =
        PortFunnels.initialState
            { localStoragePrefix = Persistence.prefix
            , cmdPort = cmdPort
            , subPort = subPort
            }
    }


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    initialModel
        |> withCmd initialSizeCmd


storageHandler : LocalStorage.Response -> State Msg -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ Persistence.readThing cmdPort <| boardIdKey currentBoardId
                    ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    case decodePersistentThing key v of
                        Err _ ->
                            mdl |> withNoCmd

                        Ok thing ->
                            case thing of
                                PersistentBoard board ->
                                    let
                                        newBoard =
                                            addPlayer mdl.player <|
                                                { board | id = currentBoardId }
                                    in
                                    { mdl | board = newBoard }
                                        |> withCmds
                                            [ Persistence.readThing
                                                cmdPort
                                              <|
                                                playerIdKey currentBoardId currentPlayerId
                                            ]

                                PersistentPlayer player ->
                                    { mdl
                                        | player = player
                                        , board =
                                            removePlayer mdl.player mdl.board
                                                |> addPlayer player
                                    }
                                        |> withCmd
                                            (Persistence.readThing cmdPort modelKey)

                                PersistentModel savedModel ->
                                    { mdl | layout = savedModel.layout }
                                        |> withNoCmd

        _ ->
            mdl |> withCmd cmd


socketHandler : WebSocket.Response -> State Msg -> Model -> ( Model, Cmd Msg )
socketHandler response state model =
    -- TODO, see `socketHandler` in ~/elm-mastodon-websocket/example/src/Main.elm
    model |> withNoCmd


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : State Msg -> String -> model -> (Value -> Cmd Msg)
getCmdPort state moduleName model =
    cmdPort


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


saveModel : Model -> ( Model, Cmd Msg )
saveModel model =
    model |> withCmd (writeModel model cmdPort)


editMaze : Model -> ( Model, Cmd Msg )
editMaze model =
    saveModel { model | layout = EditingLayout }


{-| This reverts to the default for now.

Will eventually bring up a dialog to select a saved maze.

-}
getMaze : Model -> ( Model, Cmd Msg )
getMaze model =
    let
        board =
            { simpleBoard | id = currentBoardId }

        player =
            model.player

        ( x, y ) =
            player.location

        newPlayer =
            if x >= board.rows || y >= board.cols then
                { player
                    | location = ( 0, 0 )
                    , direction = South
                }

            else
                player

        newBoard =
            addPlayer newPlayer board

        mdl =
            { model
                | board = newBoard
                , layout = TopViewLayout
            }
    in
    mdl
        |> withCmds
            [ chainWrites
                [ WriteBoard newBoard
                , WritePlayer newPlayer
                , WriteModel mdl
                ]
            ]


{-| For now, this just exits editing mode.

Will eventually bring up a dialog to type a name for saving.

-}
saveMaze : Model -> ( Model, Cmd Msg )
saveMaze model =
    saveModel { model | layout = TopViewLayout }


toggleLayout : Model -> ( Model, Cmd Msg )
toggleLayout model =
    let
        layout =
            case model.layout of
                NormalLayout ->
                    TopViewLayout

                _ ->
                    NormalLayout

        mdl =
            { model | layout = layout }
    in
    mdl |> withCmd (writeModel mdl cmdPort)


toggleWall : Direction -> Location -> Model -> ( Model, Cmd Msg )
toggleWall direction location model =
    let
        board =
            model.board
    in
    case getCell location board of
        Nothing ->
            model |> withNoCmd

        Just cell ->
            let
                ( r, c ) =
                    location

                walls =
                    cell.walls

                newWalls =
                    case direction of
                        West ->
                            { walls | west = not walls.west }

                        North ->
                            { walls | north = not walls.north }

                        _ ->
                            walls

                northCell =
                    case getCell ( r - 1, c ) board of
                        Nothing ->
                            Nothing

                        Just nc ->
                            let
                                nw =
                                    nc.walls
                            in
                            Just
                                { nc
                                    | walls =
                                        { nw
                                            | south = newWalls.north
                                        }
                                }

                westCell =
                    case getCell ( r, c - 1 ) board of
                        Nothing ->
                            Nothing

                        Just wc ->
                            let
                                ww =
                                    wc.walls
                            in
                            Just
                                { wc
                                    | walls =
                                        { ww
                                            | east = newWalls.west
                                        }
                                }

                newCell =
                    { cell | walls = newWalls }

                newBoard =
                    setCell location newCell model.board

                nb2 =
                    case northCell of
                        Nothing ->
                            newBoard

                        Just nc ->
                            setCell ( r - 1, c ) nc newBoard

                nb3 =
                    case westCell of
                        Nothing ->
                            nb2

                        Just wc ->
                            setCell ( r, c - 1 ) wc nb2

                mdl =
                    { model | board = nb3 }
            in
            mdl |> withCmd (writeBoard nb3 cmdPort)


changeBoardSize : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
changeBoardSize ( rowinc, colinc ) model =
    let
        board =
            model.board

        size =
            ( rowinc + board.rows, colinc + board.cols )

        nb =
            Board.resize size board

        mdl =
            { model
                | board = nb
                , player = Board.fixPlayer nb model.player
            }
    in
    ( mdl
    , chainWrites
        [ WriteBoard mdl.board
        , WritePlayer mdl.player
        ]
    )


chainWrites : List Write -> Cmd Msg
chainWrites writes =
    Task.perform DoWrites <| Task.succeed writes


doWrite : Write -> Cmd msg
doWrite write =
    case write of
        WriteBoard board ->
            writeBoard board cmdPort

        WritePlayer player ->
            writePlayer player cmdPort

        WriteModel model ->
            writeModel model cmdPort


updateButton : Button Operation -> Model -> Model
updateButton button model =
    case Button.getState button of
        GoForward ->
            { model | forwardButton = button }

        GoBack ->
            { model | backButton = button }

        _ ->
            model


dummyButton : Button Operation
dummyButton =
    Button.simpleButton ( 0, 0 ) TurnRight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        ReceiveTask result ->
            -- TODO
            model |> withNoCmd

        DoWrites writes ->
            case writes of
                [] ->
                    model |> withNoCmd

                write :: rest ->
                    model
                        |> withCmds
                            [ doWrite write
                            , chainWrites rest
                            ]

        RepeatButtonMsg time operation m ->
            if time >= model.delayLeft then
                update (ButtonMsg operation m)
                    { model | delayLeft = 0 }

            else
                { model | delayLeft = model.delayLeft - time }
                    |> withNoCmd

        ButtonMsg operation m ->
            let
                msgButton =
                    case operation of
                        GoForward ->
                            model.forwardButton

                        GoBack ->
                            model.backButton

                        _ ->
                            Button.simpleButton ( 0, 0 ) operation
            in
            case Button.checkSubscription m msgButton of
                Just ( time, m2 ) ->
                    { model
                        | subscription =
                            if time <= 0 then
                                Nothing

                            else
                                Just ( time, operation, m2 )
                        , delayLeft =
                            if time <= 0 then
                                0

                            else
                                time
                    }
                        |> withNoCmd

                Nothing ->
                    let
                        ( isClick, button, cmd ) =
                            Button.update (\bm -> ButtonMsg operation bm)
                                m
                                msgButton

                        dir =
                            operationToDirection operation

                        ( mdl, cmd2 ) =
                            if isClick then
                                case operation of
                                    ToggleLayout ->
                                        toggleLayout model

                                    ToggleWall direction location ->
                                        toggleWall direction location model

                                    AddRow count ->
                                        changeBoardSize ( count, 0 ) model

                                    AddColumn count ->
                                        changeBoardSize ( 0, count ) model

                                    EditMaze ->
                                        editMaze model

                                    GetMaze ->
                                        getMaze model

                                    SaveMaze ->
                                        saveMaze model

                                    _ ->
                                        let
                                            mdl2 =
                                                movePlayer dir model
                                        in
                                        mdl2
                                            |> withCmd
                                                (writePlayer mdl2.player cmdPort)

                            else
                                model |> withNoCmd
                    in
                    updateButton button
                        { mdl
                            | isTouchAware =
                                if Button.isTouchAware button then
                                    True

                                else
                                    mdl.isTouchAware
                        }
                        |> withCmds [ cmd, cmd2 ]

        InitialSize size ->
            { model | windowSize = size }
                |> withNoCmd

        Resize size ->
            { model | windowSize = size } |> withNoCmd

        DownKey key ->
            let
                mdl =
                    processDownKey key model
            in
            mdl |> withCmd (writePlayer mdl.player cmdPort)

        OnUrlRequest _ ->
            model |> withNoCmd

        OnUrlChange _ ->
            model |> withNoCmd

        Nop ->
            model |> withNoCmd


moveDelta : Direction -> Direction -> ( Int, Int )
moveDelta dir playerDir =
    if dir == North then
        case playerDir of
            North ->
                ( -1, 0 )

            South ->
                ( 1, 0 )

            East ->
                ( 0, 1 )

            West ->
                ( 0, -1 )

    else
        case playerDir of
            North ->
                ( 1, 0 )

            South ->
                ( -1, 0 )

            East ->
                ( 0, -1 )

            West ->
                ( 0, 1 )


moveDir : Direction -> Direction -> Direction
moveDir dir playerDir =
    if dir == East then
        case playerDir of
            North ->
                East

            East ->
                South

            South ->
                West

            West ->
                North

    else
        case playerDir of
            North ->
                West

            West ->
                South

            South ->
                East

            East ->
                North


movePlayer : Direction -> Model -> Model
movePlayer dir model =
    let
        player =
            model.player

        playerDir =
            player.direction

        loc =
            player.location

        ( r, c ) =
            loc

        board =
            model.board

        ( rows, cols ) =
            ( board.rows, board.cols )

        ( newloc, newdir ) =
            if dir == North || dir == South then
                let
                    ( dr, dc ) =
                        moveDelta dir playerDir
                in
                if canMove loc ( dr, dc ) board then
                    ( ( r + dr, c + dc ), playerDir )

                else
                    ( loc, playerDir )

            else
                ( loc, moveDir dir playerDir )
    in
    if newloc == loc && newdir == playerDir then
        model

    else
        let
            newPlayer =
                { player
                    | location = newloc
                    , direction = newdir
                }

            newBoard =
                updatePlayer player newPlayer board
        in
        { model
            | board = newBoard
            , player = newPlayer
        }


upChars : List String
upChars =
    [ "i", "I", "w", "W" ]


downChars : List String
downChars =
    [ "k", "K", "s", "S" ]


rightChars : List String
rightChars =
    [ "l", "L", "d", "D" ]


leftChars : List String
leftChars =
    [ "j", "J", "a", "A" ]


processDownKey : String -> Model -> Model
processDownKey key model =
    let
        dir =
            if List.member key upChars then
                Just North

            else if List.member key downChars then
                Just South

            else if List.member key rightChars then
                Just East

            else if List.member key leftChars then
                Just West

            else
                Nothing
    in
    case dir of
        Nothing ->
            model

        Just d ->
            movePlayer d model


br : Html msg
br =
    Html.br [] []


lines : List String -> Html Msg
lines strings =
    p [] (List.concatMap (\s -> [ text s, br ]) strings)


sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []


logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a [ href url ]
        [ sqrimg ("images/" ++ img) name size ]


mailLink : String -> Html Msg
mailLink email =
    span []
        [ text "<"
        , a [ href ("mailto:" ++ email) ]
            [ text email ]
        , text ">"
        ]


space : Html Msg
space =
    text " "


renderContent : Model -> Html Msg
renderContent model =
    let
        ws =
            model.windowSize

        w =
            0.9 * min ws.width (ws.height * 2 / 3)

        ta =
            model.isTouchAware

        ( r1, r2 ) =
            case model.layout of
                NormalLayout ->
                    ( render3d ta, render2d False ta )

                EditingLayout ->
                    ( render2d True ta, render3d ta )

                _ ->
                    ( render2d False ta, render3d ta )
    in
    div []
        [ r1 w False model.player model.board
        , br
        , r2 (w / 3) True model.player model.board
        , space
        , renderControls (w / 3)
            model.isTouchAware
            model.layout
            model.forwardButton
            model.backButton
        ]


view : Model -> Document Msg
view model =
    { title = "MineSpace"
    , body =
        if model.layout == NoLayout then
            [ div [] [] ]

        else
            [ div [ align "center" ]
                [ Styles.style
                , h2 []
                    [ text "MineSpace" ]
                , renderContent model
                , p []
                    [ text "Use IJKL or WASD to move/rotate."
                    , br
                    , text "Click in the small maze view to make it big."
                    ]
                , p []
                    [ text "Server coming soon. " ]
                , p []
                    [ logoLink "https://github.com/billstclair/minespace"
                        "GitHub-Mark-32px.png"
                        "GitHub source code"
                        32
                    , space
                    , logoLink "http://elm-lang.org/"
                        "elm-logo-125x125.png"
                        "Elm inside"
                        28
                    , br
                    , text (copyright ++ " 2018 ")
                    , a [ href "https://GibGoyGames.com/" ]
                        [ text "Gib Goy Games" ]
                    , space
                    , mailLink "GibGoyGames@gmail.com"
                    ]
                ]
            ]
    }


keyDecoder : Decoder Msg
keyDecoder =
    JD.field "key" JD.string
        |> JD.map DownKey


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize (\w h -> Resize <| Size (toFloat w) (toFloat h))
        , Events.onKeyDown keyDecoder
        , Time.every delay
            (\_ ->
                case model.subscription of
                    Nothing ->
                        Noop

                    Just ( time, operation, msg ) ->
                        RepeatButtonMsg delay operation msg
            )
        ]
