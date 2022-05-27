module Icon exposing (download, moon)

import Html.Attributes exposing (attribute)
import Svg exposing (..)
import Svg.Attributes exposing (..)


moon : Int -> Svg msg
moon height_ =
    svg
        [ viewBox "0 0 32 32"
        , attribute "role" "img"
        , height (String.fromInt height_)
        , fill "none"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        ]
        [ Svg.path [ d "M14 2C 9 2 3 7 3 15 3 23 9 29 17 29 25 29 30 23 30 18 19 25 7 13 14 2Z" ] [] ]


download : Int -> Svg msg
download height_ =
    svg
        [ viewBox "0 0 24 24"
        , attribute "role" "img"
        , height (String.fromInt height_)
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path [ d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4M7 10l5 5 5-5M12 15V3" ] [] ]
