----------------------------------------------------------------------
--
-- Styles.elm
-- The CSS Stylesheet for the MinePlace game.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module MinePlace.Styles exposing
    ( SClass(..)
    , class
    , classes
    , style
    )

import Css exposing (Sel(..))
import Html.Attributes
import MinePlace.Types exposing (Colors)


type SClass
    = Error
      -- SVG Classes
    | SvgBorder
    | SvgLine
    | Svg2dPlayer
    | SvgEditorHighlight
    | WindowText


imports : List String
imports =
    []


rule : a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
    { selectors = selectors
    , descriptor = descriptor
    }


greenColor : String
greenColor =
    "#E0FFE0"


rules colors =
    [ rule
        [ Class Error ]
        [ ( "background-color", colors.errorBackground )
        , ( "color", colors.errorColor )
        ]

    -- SVG classes
    , rule
        [ Class SvgBorder ]
        [ ( "fill-opacity", "1.0" )
        , ( "stroke", colors.borderStroke )
        , ( "fill", colors.borderFill )
        , ( "stroke-width", "2px" )
        ]
    , rule
        [ Class SvgLine ]
        [ ( "stroke", colors.lineStroke )
        , ( "stroke-width", "2px" )
        ]
    , rule
        [ Class Svg2dPlayer ]
        [ ( "stroke", colors.playerStroke )
        , ( "stroke-width", "1px" )
        , ( "fill", colors.playerFill )
        ]
    , rule
        [ Class SvgEditorHighlight ]
        [ ( "stroke", colors.editorHighlightStroke )
        , ( "fill", colors.editorHighlightFill )
        , ( "stroke-width", "1px" )
        , ( "stroke-opacity", "0.1" )
        , ( "fill-opacity", "0.1" )
        ]
    , rule
        [ Class WindowText ]
        [ ( "background", colors.windowBackground )
        , ( "color", colors.textColor )
        ]
    ]


stylesheet colors =
    Css.stylesheet imports <| rules colors



-- This is for inclusion at the beginning of the Board div


style colors =
    Css.style [] <| stylesheet colors



-- For use in the attributes of Html elements


class colors =
    .class <| stylesheet colors


classes colors =
    .classes <| stylesheet colors
