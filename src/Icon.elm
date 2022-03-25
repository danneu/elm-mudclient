module Icon exposing (..)

import Html
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr



-- SVG icons used in the UI.


moonSvg : Html.Html msg
moonSvg =
    svg
        [ SvgAttr.viewBox "0 0 32 32"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.class "svg-icon"
        ]
        [ path
            [ SvgAttr.d "M14 2C 9 2 3 7 3 15 3 23 9 29 17 29 25 29 30 23 30 18 19 25 7 13 14 2Z"
            ]
            []
        ]


downloadSvg : Html.Html msg
downloadSvg =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.class "feather feather-download svg-icon"
        ]
        [ path
            [ SvgAttr.d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"
            ]
            []
        , Svg.polyline
            [ SvgAttr.points "7 10 12 15 17 10"
            ]
            []
        , Svg.line
            [ SvgAttr.x1 "12"
            , SvgAttr.y1 "15"
            , SvgAttr.x2 "12"
            , SvgAttr.y2 "3"
            ]
            []
        ]
