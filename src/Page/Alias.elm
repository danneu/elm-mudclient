module Page.Alias exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra



{- Aliases

   For now, aliases are ultra-simple static replacements like:

       "w": "go west"
       "hp": "open bag\nget food\neat food"

   But I would like to eventually support variable replacement like:

       -- Imagine spells that need to be prepared before casting
       "c * *": "prepare $1\ncast $1 $2"
       -- "c heal gandalf" -> "prepare heal; cast heal gandalf"

   Syntax to be decided later, if I ever get there.
-}


type alias Model =
    { aliases : List ( String, String )
    }


init : List ( String, String ) -> Model
init aliases =
    { aliases = aliases }


type Msg
    = AddAlias String String
    | DeleteAlias Int
    | NewAlias
    | UpdateAlias Int String String
    | Save
    | Close


type OutMsg
    = NoOp
    | Exit
    | SetAliases (List ( String, String ))


update : Msg -> Model -> ( ( Model, Cmd Msg ), OutMsg )
update msg model =
    case msg of
        DeleteAlias idx ->
            let
                newAliases =
                    List.Extra.removeAt idx model.aliases
            in
            ( ( { model | aliases = newAliases }, Cmd.none )
            , NoOp
            )

        NewAlias ->
            let
                emptyAlias =
                    ( "", "" )
            in
            ( ( { model | aliases = model.aliases ++ [ emptyAlias ] }, Cmd.none )
            , NoOp
            )

        AddAlias k v ->
            let
                newAliases =
                    model.aliases ++ [ ( k, v ) ]
            in
            ( ( { model | aliases = newAliases }, Cmd.none )
            , NoOp
            )

        UpdateAlias idx newK newV ->
            let
                newAliases =
                    List.Extra.updateAt idx (\_ -> ( newK, newV )) model.aliases
            in
            ( ( { model | aliases = newAliases }, Cmd.none )
            , NoOp
            )

        Save ->
            ( ( model, Cmd.none )
              -- Trim only on save so that user can have trailing \n when writing aliases
            , SetAliases
                (List.map
                    (\( k, v ) ->
                        ( String.trim k, collapseNewlines v )
                    )
                    model.aliases
                )
            )

        Close ->
            ( ( model, Cmd.none )
            , Exit
            )


view : Model -> Html Msg
view model =
    div
        [ class "alias-page" ]
        [ button
            [ onClick Close
            , style "float" "right"
            ]
            [ text "Close" ]
        , h2 [] [ text "Aliases" ]
        , hr [] []
        , p [] [ text "Aliases let you type in shortcuts that get expanded into longer/multiple messages that will be sent to the server." ]
        , table
            []
            [ thead
                []
                [ tr []
                    [ th [] [ text "Key" ]
                    , th [] [ text "Value" ]
                    , th [] []
                    ]
                ]
            , tbody
                []
                (List.concat
                    [ if List.isEmpty model.aliases then
                        [ tr
                            []
                            [ td []
                                [ pre [] [ text "loot" ] ]
                            , td []
                                [ pre [] [ text "open corpse\ntake all from corpse\nbury corpse" ] ]
                            , td [] [ text "(Example)" ]
                            ]
                        ]

                      else
                        []
                    , Tuple.second <|
                        List.foldl
                            (\( k, v ) ( idx, acc ) ->
                                let
                                    element =
                                        tr
                                            []
                                            [ td []
                                                [ input
                                                    [ value k
                                                    , classList
                                                        [ ( "blink-error-bg", isInvalidKey k ) ]
                                                    , onInput (\newK -> UpdateAlias idx newK v)
                                                    ]
                                                    []
                                                ]
                                            , td []
                                                [ textarea
                                                    [ value v
                                                    , classList
                                                        [ ( "blink-error-bg", isInvalidValue v ) ]
                                                    , onInput (\newV -> UpdateAlias idx k newV)
                                                    ]
                                                    []
                                                ]
                                            , td []
                                                [ button [ onClick (DeleteAlias idx) ] [ text "Delete" ] ]
                                            ]
                                in
                                ( idx + 1, acc ++ [ element ] )
                            )
                            ( 0, [] )
                            model.aliases
                    ]
                )
            , button
                [ onClick NewAlias ]
                [ text "New alias" ]
            ]
        , button
            [ class "primary"
            , onClick Save
            ]
            [ text "Save" ]
        ]



-- ALIAS VALIDATION


{-| Clean up consecutive whitespace.

        collapseNewlines "  a \n \n b  \n\n c\n " == "a\nb\nc"

-}
collapseNewlines : String -> String
collapseNewlines text =
    text
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


isInvalidKey : String -> Bool
isInvalidKey k =
    String.isEmpty (String.trim k)


isInvalidValue : String -> Bool
isInvalidValue v =
    String.isEmpty (String.trim v)
