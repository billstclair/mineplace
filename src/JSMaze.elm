----------------------------------------------------------------------
--
-- JSMaze.elm
-- Simple maze game.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze exposing (..)

import Char
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
import JSMaze.Board exposing (addPlayer, canMove, simpleBoard, updatePlayer)
import JSMaze.Render exposing (render2d, render3d, renderControls)
import JSMaze.SharedTypes
    exposing
        ( Board
        , Direction(..)
        , Msg(..)
        , Player
        , operationToDirection
        )
import JSMaze.Styles as Styles
import Keyboard exposing (KeyCode)
import List.Extra as LE
import Svg.Button as Button
import Task
import Time exposing (Time)
import Window exposing (Size)


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\x -> InitialSize x) Window.size


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Size
    , board : Board
    , player : Player
    , isTouchAware : Bool
    }


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialPlayer : Player
initialPlayer =
    { id = 0
    , name = "Joe Bob"
    , location = ( 0, 0 )
    , direction = South
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , board = addPlayer initialPlayer simpleBoard
    , player = initialPlayer
    , isTouchAware = False
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ initialSizeCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMsg msg ->
            let
                ( isClick, button, _ ) =
                    Button.update msg

                dir =
                    operationToDirection <| Button.getState button

                mdl =
                    if isClick then
                        movePlayer dir model
                    else
                        model
            in
            { mdl
                | isTouchAware =
                    if Button.isTouchAware button then
                        True
                    else
                        mdl.isTouchAware
            }
                ! []

        InitialSize size ->
            { model | windowSize = size }
                ! []

        Resize size ->
            { model | windowSize = size } ! []

        DownKey code ->
            processDownKey code model ! []

        Nop ->
            model ! []


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


upChars : List Char
upChars =
    [ 'i', 'I', 'w', 'W' ]


downChars : List Char
downChars =
    [ 'k', 'K', 's', 'S' ]


rightChars : List Char
rightChars =
    [ 'l', 'L', 'd', 'D' ]


leftChars : List Char
leftChars =
    [ 'j', 'J', 'a', 'A' ]


processDownKey : Int -> Model -> Model
processDownKey code model =
    let
        char =
            Char.fromCode code

        dir =
            if List.member char upChars then
                Just North
            else if List.member char downChars then
                Just South
            else if List.member char rightChars then
                Just East
            else if List.member char leftChars then
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


view : Model -> Html Msg
view model =
    let
        ws =
            model.windowSize

        w =
            0.6 * toFloat (min ws.width ws.height)
    in
    div [ align "center" ]
        [ Styles.style
        , h2 []
            [ text "JSMaze" ]
        , render3d w model.player model.board
        , br
        , render2d (w / 3) (Just model.player) model.board
        , text " "
        , renderControls (w / 3) model.isTouchAware
        , p []
            [ text "Use IJKL or WASD to move/rotate." ]
        , p []
            [ text "Mobile controls coming soon. " ]
        , p []
            [ a
                [ href "https://github.com/billstclair/elm-jsmaze"
                , target "_blank"
                ]
                [ text "GitHub" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs DownKey
        ]
