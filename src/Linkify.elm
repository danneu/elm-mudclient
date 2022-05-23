module Linkify exposing (html)

import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import Regex exposing (Regex)


html : String -> List (Html msg)
html content =
    let
        links =
            Regex.find linkMatcher content
                |> List.map
                    (\{ match } ->
                        a
                            [ href
                                (if String.startsWith "http://" match || String.startsWith "https://" match then
                                    match

                                 else
                                    "https://" ++ match
                                )
                            , target "_blank"
                            , rel "noopener noreferrer"
                            ]
                            [ text match ]
                    )

        rest =
            Regex.split linkMatcher content |> List.map text
    in
    List.Extra.interweave rest links


linkMatcher : Regex
linkMatcher =
    Regex.fromString "https?:\\/\\/[^\\s\"\\<\\>\\[\\]\\(\\)]+"
        |> Maybe.withDefault Regex.never
