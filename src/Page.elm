module Page exposing (Msg(..), Page(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Alias


type Page
    = AliasPage Page.Alias.Model


type Msg
    = NoOp
    | Exit
    | AliasMsg Page.Alias.OutMsg


{-| TODO: Use this instead of viewPage in Main.elm.
-}
view : Html Msg -> Html Msg
view content =
    div
        []
        [ div [ class "page-overlay" ] []
        , div [ class "page closer" ]
            [ div [ class "stage" ]
                [ div [ class "modal-container" ]
                    [ div [ class "modal-body" ]
                        [ Html.map msgMap content ]
                    ]
                ]
            ]
        ]


msgMap : Msg -> Msg
msgMap msg =
    case msg of
        AliasMsg Page.Alias.Exit ->
            Exit

        _ ->
            msg
