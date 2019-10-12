module Style exposing (..)

import Css exposing (property)
import Css.Global exposing (Snippet, selector)


reset : Snippet
reset =
    selector "*"
        [ property "padding" "0px"
        , property "margin" "0px"
        , property "box-sizing" "border-box"
        ]


mainStyle : List Snippet
mainStyle =
    [ selector "body"
        [ property "height" "100vh"
        , property "display" "flex"
        , property "justify-content" "center"
        , property "align-items" "center"
        , property "background-color" "#ddd"
        , property "font-family" "Arial, Helvetica, sans-serif"
        ]
    , selector ".game"
        [ property "display" "flex"
        , property "align-items" "center"
        ]
    , selector ".game > div"
        [ property "display" "inline-block"
        ]
    , selector ".game > div:first-child"
        [ property "margin-right" "100px"
        ]
    , selector ".game > div:last-child"
        [ property "margin-left" "100px"
        ]
    , selector ".statContainer"
        [ property "position" "relative"
        ]
    , selector ".circleContainer"
        [ property "border" "6px solid #ddd"
        , property "border-radius" "50%"
        , property "padding" "16px"
        ]
    , selector ".border"
        [ property "border-color" "#70ae6e"
        ]
    , selector ".circle"
        [ property "width" "160px"
        , property "height" "160px"
        , property "border-radius" "50%"
        , property "background-color" "#000"
        , property "display" "flex"
        , property "justify-content" "center"
        , property "align-items" "center"
        , property "font-size" "40px"
        , property "color" "#fff"
        ]
    , selector ".opponent"
        [ property "background-color" "#fff"
        , property "color" "#000"
        ]
    , selector ".restart"
        [ property "display" "flex"
        , property "justify-content" "center"
        , property "margin-top" "40px"
        ]
    , selector "button"
        [ property "border-radius" "20px"
        , property "border" "1px solid #70ae6e"
        , property "background-color" "#70ae6e"
        , property "color" "#000"
        , property "font-weight" "bold"
        , property "padding" "12px 45px"
        , property "letter-spacing" "1px"
        , property "text-transform" "uppercase"
        , property "transition" "transform 80ms ease-in"
        ]
    , selector "button:focus"
        [ property "outline" "none"
        ]
    , selector "button:active"
        [ property "transform" "scale(0.95)"
        ]
    , selector ".resultContainer"
        [ property "position" "absolute"
        , property "top" "-20%"
        , property "left" "25%"
        , property "width" "100%"
        , property "display" "none"
        ]
    , selector ".result"
        [ property "padding" "10px"
        , property "background-color" "#70ae6e"
        , property "color" "#000"
        , property "text-align" "center"
        , property "width" "50%"
        , property "border-radius" "6px"
        , property "text-transform" "uppercase"
        , property "font-weight" "bold"
        , property "font-spacing" "1px"
        ]
    , selector ".result:after"
        [ property "content" "\"\""
        , property "border-width" "10px"
        , property "border-style" "solid"
        , property "border-color" "#70ae6e transparent transparent transparent"
        , property "position" "absolute"
        , property "top" "100%"
        , property "left" "21%"
        ]
    , selector ".show"
        [ property "display" "block"
        ]
    ]
