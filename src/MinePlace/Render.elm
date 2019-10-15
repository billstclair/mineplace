----------------------------------------------------------------------
--
-- Render.elm
-- Functions for rendering a MinePlace board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module MinePlace.Render exposing
    ( getDeltaN
    , render2d
    , render3d
    , renderControls
    )

import Array exposing (Array)
import Debug exposing (log)
import Html exposing (Html)
import MinePlace.Board exposing (forwardDelta, getCell)
import MinePlace.Styles exposing (SClass(..))
import MinePlace.TwoDMath exposing (Rectangle, Vector)
import MinePlace.Types
    exposing
        ( Board
        , BoardSpec
        , Cell
        , Direction(..)
        , Layout(..)
        , Location
        , Msg(..)
        , Operation(..)
        , Player
        , Row
        , WallSpec
        , Walls
        , sumLocations
        )
import Svg exposing (Svg, g, image, line, rect, svg)
import Svg.Attributes
    exposing
        ( class
        , fill
        , fontSize
        , height
        , points
        , stroke
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
import Svg.Button as Button
    exposing
        ( Button
        , Content(..)
        )


render2d : Button.Colors -> Bool -> Bool -> Float -> Bool -> Player -> Board -> Html Msg
render2d colors forEditing isTouchAware w withToggleButton player board =
    let
        rows =
            board.rows

        cols =
            board.cols

        frows =
            toFloat rows

        fcols =
            toFloat cols

        extra =
            if forEditing then
                0.5

            else
                0

        delta =
            w / (fcols + extra)

        outerw =
            fcols * delta

        outerh =
            frows * delta

        addcolw =
            w - outerw

        addcolSize =
            ( addcolw, addcolw )

        h =
            outerh + addcolw
    in
    svg
        [ width <| fts w
        , height <| fts h
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width <| fts (outerw - 2)
            , height <| fts (outerh - 2)
            ]
            []
        , g [ class "SvgLine" ] <|
            List.indexedMap
                (render2dRow colors isTouchAware forEditing delta)
                (Array.toList board.contents)
        , if not forEditing then
            g [] []

          else
            g []
                [ Button.render
                    ( outerw - 2, 0 )
                    (TextContent "+")
                    (ButtonMsg <| AddColumn 1)
                    (simpleButton addcolSize (AddColumn 1) isTouchAware
                        |> Button.setColors colors
                    )
                , Button.render
                    ( outerw - 2, addcolw - 2 )
                    (TextContent "-")
                    (ButtonMsg <| AddColumn -1)
                    (simpleButton addcolSize (AddColumn -1) isTouchAware
                        |> Button.setColors colors
                    )
                , Button.render
                    ( 0, outerh - 2 )
                    (TextContent "+")
                    (ButtonMsg <| AddRow 1)
                    (simpleButton addcolSize (AddRow 1) isTouchAware
                        |> Button.setColors colors
                    )
                , Button.render
                    ( addcolw - 2, outerh - 2 )
                    (TextContent "-")
                    (ButtonMsg <| AddRow -1)
                    (simpleButton addcolSize (AddRow -1) isTouchAware
                        |> Button.setColors colors
                    )
                ]
        , render2dPlayer delta player
        , if withToggleButton then
            renderToggleButton isTouchAware w h

          else
            g [] []
        ]


render2dRow : Button.Colors -> Bool -> Bool -> Float -> Int -> Row -> Svg Msg
render2dRow colors isTouchAware forEditing delta rowidx row =
    g [] <|
        List.indexedMap
            (render2dCell colors isTouchAware forEditing delta rowidx)
            (Array.toList row)


render2dCell : Button.Colors -> Bool -> Bool -> Float -> Int -> Int -> Cell -> Svg Msg
render2dCell colors isTouchAware forEditing delta rowidx colidx cell =
    let
        walls =
            cell.walls

        north =
            walls.north

        west =
            walls.west

        x1f =
            delta * toFloat colidx

        y1f =
            delta * toFloat rowidx

        x1s =
            fts x1f

        y1s =
            fts y1f

        h =
            delta / 2

        q =
            h / 2

        x1fmq =
            fts <| x1f - q

        x1fpq =
            fts <| x1f + q

        y1fmq =
            fts <| y1f - q

        y1fpq =
            fts <| y1f + q

        hs =
            fts h

        g0 =
            g [] []

        westButton =
            if forEditing && colidx > 0 then
                g [ transform ("translate(" ++ x1fmq ++ " " ++ y1fpq ++ ")") ]
                    [ Svg.rect
                        [ x "0"
                        , y "0"
                        , width hs
                        , height hs
                        , class "SvgEditorHighlight"
                        ]
                        []
                    , renderOverlayButton
                        (ToggleWall West ( rowidx, colidx ))
                        isTouchAware
                        h
                        h
                    ]

            else
                g0

        northButton =
            if forEditing && rowidx > 0 then
                g [ transform ("translate(" ++ x1fpq ++ " " ++ y1fmq ++ ")") ]
                    [ Svg.rect
                        [ x "0"
                        , y "0"
                        , width hs
                        , height hs
                        , class "SvgEditorHighlight"
                        ]
                        []
                    , renderOverlayButton
                        (ToggleWall North ( rowidx, colidx ))
                        isTouchAware
                        h
                        h
                    ]

            else
                g0

        northLine =
            if north && rowidx > 0 then
                Svg.line
                    [ x1 x1s
                    , x2 (fts <| x1f + delta)
                    , y1 y1s
                    , y2 y1s
                    ]
                    []

            else
                g0

        westLine =
            if west && colidx > 0 then
                Svg.line
                    [ x1 x1s
                    , x2 x1s
                    , y1 y1s
                    , y2 (fts <| y1f + delta)
                    ]
                    []

            else
                g0
    in
    if north || forEditing then
        if west || forEditing then
            g [] [ northLine, westLine, northButton, westButton ]

        else
            northLine

    else
        westLine


render2dPlayer : Float -> Player -> Svg Msg
render2dPlayer delta player =
    let
        q =
            0.2 * delta

        h =
            0.5 * delta

        t =
            0.25 * delta

        ( r, c ) =
            player.location

        x =
            delta * toFloat c

        y =
            delta * toFloat r

        ( ( x1f, y1f ), ( x2f, y2f ), ( x3f, y3f ) ) =
            case player.direction of
                North ->
                    ( ( delta - t, delta - q ), ( t, delta - q ), ( h, q ) )

                South ->
                    ( ( t, q ), ( delta - t, q ), ( h, delta - q ) )

                East ->
                    ( ( q, delta - t ), ( q, t ), ( delta - q, h ) )

                West ->
                    ( ( delta - q, t ), ( delta - q, delta - t ), ( q, h ) )

        ( ( x1s, y1s ), ( x2s, y2s ), ( x3s, y3s ) ) =
            ( ( fts <| x + x1f
              , fts <| y + y1f
              )
            , ( fts <| x + x2f
              , fts <| y + y2f
              )
            , ( fts <| x + x3f
              , fts <| y + y3f
              )
            )

        p1 =
            x1s ++ "," ++ y1s

        p2 =
            x2s ++ "," ++ y2s

        p3 =
            x3s ++ "," ++ y3s
    in
    Svg.polygon
        [ class "Svg2dPlayer"
        , points <| p1 ++ " " ++ p2 ++ " " ++ p3
        ]
        []


renderToggleButton =
    renderOverlayButton ToggleLayout


renderOverlayButton : Operation -> Bool -> Float -> Float -> Svg Msg
renderOverlayButton operation isTouchAware width height =
    let
        button =
            simpleButton ( width, height ) operation isTouchAware
    in
    Button.renderOverlay (ButtonMsg operation) button


vanishingDistance : Int
vanishingDistance =
    12


vanishingSize : Float
vanishingSize =
    0.2


wallLength : Float
wallLength =
    sqrt (2 * (((1 - vanishingSize) / 2) ^ 2))


wallConstant : Float
wallConstant =
    0.8


sumXn : Float -> Int -> Float
sumXn x n =
    if n == 0 then
        0

    else
        let
            loop i xn res =
                if i <= 0 then
                    res

                else
                    loop (i - 1) (xn * x) (res + xn)
        in
        loop n x 0


xnConstant : Float
xnConstant =
    wallLength
        / sumXn wallConstant vanishingDistance


deltaN : Float -> Int -> Float
deltaN size n =
    let
        diag =
            xnConstant
                * sumXn wallConstant n
    in
    size * sqrt (diag ^ 2 / 2)


deltaNs : Array Float
deltaNs =
    List.map
        (deltaN 1)
        (List.range 1 vanishingDistance)
        |> Array.fromList


getDeltaN : Float -> Int -> Float
getDeltaN size n =
    case Array.get n deltaNs of
        Just dn ->
            size * dn

        Nothing ->
            deltaN size n


type alias RenderCell =
    { n : Int
    , lastdn : Float
    , dn : Float
    , left : Bool
    , right : Bool
    , leftright : Bool
    , rightleft : Bool
    , end : Bool
    }


computeRenderCells : Float -> Player -> Board -> List RenderCell
computeRenderCells size player board =
    let
        dir =
            player.direction

        loc =
            player.location

        ( ( delta, leftd, rightd ), ( getl, getr, getf ) ) =
            forwardDelta dir

        ( ( dl, _, _ ), ( _, getlr, _ ) ) =
            forwardDelta leftd

        ( ( dr, _, _ ), ( getrl, _, _ ) ) =
            forwardDelta rightd

        loop : Int -> Float -> Location -> List RenderCell -> List RenderCell
        loop n lastdn lo res =
            case getCell lo board of
                Nothing ->
                    List.reverse res

                Just cell ->
                    let
                        dn =
                            getDeltaN size n

                        walls =
                            cell.walls

                        left =
                            getl walls

                        right =
                            getr walls

                        locl =
                            sumLocations lo dl

                        locr =
                            sumLocations lo dr

                        leftright =
                            if left then
                                False

                            else
                                case getCell locl board of
                                    Nothing ->
                                        False

                                    Just lc ->
                                        getlr lc.walls

                        rightleft =
                            if right then
                                False

                            else
                                case getCell locr board of
                                    Nothing ->
                                        False

                                    Just rc ->
                                        getrl rc.walls

                        end =
                            getf walls

                        renderCell =
                            { n = n
                            , lastdn = lastdn
                            , dn = dn
                            , left = left
                            , right = right
                            , leftright = leftright
                            , rightleft = rightleft
                            , end = end
                            }

                        nextloc =
                            sumLocations lo delta

                        nextres =
                            renderCell :: res
                    in
                    if end then
                        List.reverse nextres

                    else
                        loop (n + 1) dn nextloc nextres
    in
    loop 1 0 loc []


fts : Float -> String
fts =
    String.fromFloat


render3dCell : Button.Colors -> Float -> RenderCell -> List (Svg Msg)
render3dCell colors w cell =
    let
        ldn =
            cell.lastdn

        dn =
            cell.dn

        end =
            cell.end

        ( lltx, llty ) =
            ( fts ldn, fts ldn )

        ( llbx, llby ) =
            ( fts ldn, fts <| w - ldn )

        ( ltx, lty ) =
            ( fts dn, fts dn )

        ( lbx, lby ) =
            ( fts dn, fts <| w - dn )

        ( rltx, rlty ) =
            ( fts <| w - ldn, fts ldn )

        ( rlbx, rlby ) =
            ( fts <| w - ldn, fts <| w - ldn )

        ( rtx, rty ) =
            ( fts <| w - dn, fts dn )

        ( rbx, rby ) =
            ( fts <| w - dn, fts <| w - dn )
    in
    [ if cell.left then
        [ Svg.line [ x1 lltx, y1 llty, x2 ltx, y2 lty ] []
        , Svg.line [ x1 llbx, y1 llby, x2 lbx, y2 lby ] []
        , if end then
            Svg.line [ x1 ltx, y1 lty, x2 lbx, y2 lby ] []

          else
            g [] []
        ]

      else
        [ Svg.line [ x1 lltx, y1 llty, x2 llbx, y2 llby ] []
        , if not end then
            Svg.line [ x1 ltx, y1 lty, x2 lbx, y2 lby ] []

          else
            g [] []
        ]
    , if cell.leftright || (end && not cell.left) then
        [ Svg.line [ x1 ltx, y1 lty, x2 lltx, y2 lty ] []
        , Svg.line [ x1 lbx, y1 lby, x2 llbx, y2 lby ] []
        ]

      else if not cell.left then
        let
            tx =
                fts (dn - (0.25 * (dn - ldn)))
        in
        [ Svg.line [ x1 ltx, y1 lty, x2 tx, y2 lty ] []
        , Svg.line [ x1 tx, y1 lty, x2 tx, y2 lby ] []
        , Svg.line [ x1 tx, y1 lby, x2 lbx, y2 lby ] []
        ]

      else
        []
    , if cell.right then
        [ Svg.line [ x1 rltx, y1 rlty, x2 rtx, y2 rty ] []
        , Svg.line [ x1 rlbx, y1 rlby, x2 rbx, y2 rby ] []
        , if end then
            Svg.line [ x1 rtx, y1 rty, x2 rbx, y2 rby ] []

          else
            g [] []
        ]

      else
        [ Svg.line [ x1 rltx, y1 rlty, x2 rlbx, y2 rlby ] []
        , if not end then
            Svg.line [ x1 rtx, y1 rty, x2 rbx, y2 rby ] []

          else
            g [] []
        ]
    , if cell.rightleft || (end && not cell.right) then
        [ Svg.line [ x1 rtx, y1 rty, x2 rltx, y2 rty ] []
        , Svg.line [ x1 rbx, y1 rby, x2 rlbx, y2 rby ] []
        ]

      else if not cell.right then
        let
            tx =
                fts (w - dn + (0.25 * (dn - ldn)))
        in
        [ Svg.line [ x1 rtx, y1 rty, x2 tx, y2 rty ] []
        , Svg.line [ x1 tx, y1 rty, x2 tx, y2 rby ] []
        , Svg.line [ x1 tx, y1 rby, x2 rbx, y2 rby ] []
        ]

      else
        []
    , if end then
        [ Svg.line [ x1 ltx, y1 lty, x2 rtx, y2 rty ] []
        , Svg.line [ x1 lbx, y1 lby, x2 rbx, y2 rby ] []
        ]

      else
        []
    ]
        |> List.concat


render3d : Button.Colors -> Bool -> Float -> Bool -> Player -> Board -> Html Msg
render3d colors isTouchAware w withToggleButton player board =
    let
        ws =
            fts w

        wm2s =
            fts (w - 2)

        renderCells =
            computeRenderCells w player board

        cells =
            List.concatMap (render3dCell colors w) renderCells
    in
    svg
        [ width ws
        , height ws
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width wm2s
            , height wm2s
            ]
            []
        , g [ class "SvgLine" ]
            cells
        , if withToggleButton then
            renderToggleButton isTouchAware w w

          else
            g [] []
        ]


simpleButton : Button.Size -> Operation -> Bool -> Button Operation
simpleButton size operation isTouchAware =
    let
        button =
            Button.simpleButton size operation
    in
    Button.setTouchAware isTouchAware button


renderControls : Button.Colors -> Float -> Bool -> Layout -> Button Operation -> Button Operation -> Html Msg
renderControls colors w isTouchAware layout forwardButton reverseButton =
    let
        ws =
            fts w

        bw =
            w / 3

        bw2s =
            fts (bw * 2)

        leftRightY =
            if layout == NormalLayout then
                bw / 2 + 2

            else
                2

        size : Button.Size
        size =
            ( bw, bw )
    in
    svg
        [ width ws
        , height bw2s
        ]
        [ Button.render
            ( 2, leftRightY )
            (TextContent "<")
            (ButtonMsg TurnLeft)
            (simpleButton size TurnLeft isTouchAware
                |> Button.setColors colors
            )
        , Button.render
            ( 2 * bw - 2, leftRightY )
            (TextContent ">")
            (ButtonMsg TurnRight)
            (simpleButton size TurnRight isTouchAware
                |> Button.setColors colors
            )
        , Button.render
            ( bw, 2 )
            (TextContent "^")
            (ButtonMsg GoForward)
            (Button.setSize size forwardButton
                |> Button.setColors colors
            )
        , Button.render
            ( bw, bw )
            (TextContent "v")
            (ButtonMsg GoBack)
            (Button.setSize size reverseButton
                |> Button.setColors colors
            )
        , case layout of
            TopViewLayout ->
                g []
                    [ Button.render
                        ( 2, bw )
                        (TextContent "Edit")
                        (ButtonMsg EditMaze)
                        (simpleButton size EditMaze isTouchAware
                            |> Button.setColors colors
                        )
                    , Button.render
                        ( 2 * bw - 2, bw )
                        (TextContent "Get")
                        (ButtonMsg GetMaze)
                        (simpleButton size GetMaze isTouchAware
                            |> Button.setColors colors
                        )
                    ]

            EditingLayout ->
                g []
                    [ Button.render
                        ( 2, bw )
                        (TextContent "Save")
                        (ButtonMsg SaveMaze)
                        (simpleButton size SaveMaze isTouchAware
                            |> Button.setColors colors
                        )
                    , Button.render
                        ( 2 * bw - 2, bw )
                        (TextContent "Init")
                        (ButtonMsg GetMaze)
                        (simpleButton size GetMaze isTouchAware
                            |> Button.setColors colors
                        )
                    ]

            _ ->
                g [] []
        ]
