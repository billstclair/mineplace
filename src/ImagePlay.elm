module ImagePlay exposing (main)

{-| This shows proper sizing of images to put them in the center of a
rectangle.

SVG transforms do not yet include perspective.

In order to fake the perspective, they need to be transformed with CSS
as per:

<https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Transforms/Using_CSS_transforms>

I think it will work to draw the image with SVG, then use CSS on the
resulting HTML to do the perspective, then put that back in the SVG
rendering with <foreignObject>.

-}

import Browser
import Html exposing (Html, div, input, p, span, text)
import Html.Attributes exposing (checked, name, type_)
import Html.Events exposing (onClick)
import Svg exposing (Svg, image, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , height
        , preserveAspectRatio
        , stroke
        , width
        , x
        , xlinkHref
        , y
        )


type Image
    = Lion
    | Tree
    | Earth


type alias Model =
    { image : Image
    }


type Msg
    = SetImage Image


imageUrl : Image -> String
imageUrl image =
    case image of
        Lion ->
            "https://upload.wikimedia.org/wikipedia/commons/7/73/Lion_waiting_in_Namibia.jpg"

        Tree ->
            "https://upload.wikimedia.org/wikipedia/commons/e/eb/Ash_Tree_-_geograph.org.uk_-_590710.jpg"

        Earth ->
            "https://upload.wikimedia.org/wikipedia/commons/9/97/The_Earth_seen_from_Apollo_17.jpg"


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { image = Lion
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetImage image ->
            { model | image = image }


imageRadio : Image -> String -> Model -> Html Msg
imageRadio image label model =
    span []
        [ input
            [ type_ "radio"
            , checked <| model.image == image
            , onClick <| SetImage image
            ]
            []
        , text " "
        , text label
        ]


w : Int
w =
    768


h : Int
h =
    768


tos : Int -> String
tos x =
    String.fromInt x


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ imageRadio Lion "Lion" model
            , text " "
            , imageRadio Tree "Tree" model
            , text " "
            , imageRadio Earth "Earth" model
            ]
        , p []
            [ svg
                [ width <| tos (w + 2)
                , height <| tos (h + 2)
                ]
                [ rect
                    [ x "1"
                    , y "1"
                    , width <| tos w
                    , height <| tos h
                    , fill "white"
                    , stroke "black"
                    ]
                    []
                , image
                    [ x "2"
                    , y "2"
                    , width <| tos (w - 2)
                    , height <| tos (h - 2)
                    , xlinkHref <| imageUrl model.image
                    , preserveAspectRatio "xMidYMid meet"
                    ]
                    []
                ]
            ]
        ]
