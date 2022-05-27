port module Main exposing (..)

import Alias exposing (Alias)
import Ansi
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Icon
import Json.Decode as JD
import Linkify
import Page
import Page.Alias
import Regex
import Util



-- PORTS


{-| Sends (MUD server, telnet proxy url) to JS.
-}
port connect : ( Server, String ) -> Cmd msg


port disconnect : () -> Cmd msg


port downloadMessages : ( String, String ) -> Cmd msg


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


port websocketStateReceiver : (String -> msg) -> Sub msg



-- MODEL


type
    Chunk
    -- The subset of Ansi.Action that we care about
    = AnsiPrint AnsiState String
    | AnsiLinebreak


type alias Message =
    { text : String
    , chunks : List Chunk
    }


type alias Server =
    { host : String
    , port_ : Maybe Int
    }


{-| Quick and dirty validation for now since it's really just for dev purposes.

    Valid with or without port, but must start with ws:// or wss://

    valid: ws://localhost
    valid: wss://localhost
    valid: ws://localhost:8080
    invalid: http://example.com

-}
isValidProxyUrl : String -> Bool
isValidProxyUrl text =
    let
        regex =
            Regex.fromString "^wss?://[^:]+(?::\\d+)?$"
                |> Maybe.withDefault Regex.never
    in
    Regex.contains regex text


type ConnectionState
    = Disconnected
    | Connecting
    | Connected



-- type Page
--     = AliasPage


type alias Model =
    { draft : String
    , page : Maybe Page.Page
    , aliases : List Alias
    , ansiState : AnsiState
    , remainder : String
    , messages : List Message
    , connectionState : ConnectionState
    , server : Server
    , proxy : String
    , isDarkMode : Bool
    }


type alias Flags =
    { isDarkMode : Bool
    , proxy : Maybe String
    }


prodProxy : String
prodProxy =
    "wss://telnet-proxy.fly.dev"


devProxy : String
devProxy =
    "ws://localhost:8888"


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { draft = ""
      , page = Nothing
      , aliases = []
      , ansiState = defaultAnsiState
      , remainder = ""
      , messages = []
      , connectionState = Disconnected
      , isDarkMode = flags.isDarkMode
      , server = Server "elephant.org" (Just 23)

      --   , proxy = "ws://localhost:8080"
      -- Note: Must be wss:// if client served from https://
      , proxy = Maybe.withDefault prodProxy flags.proxy
      }
    , Cmd.none
    )



-- ANSI


type alias AnsiState =
    { bg : Maybe Ansi.Color
    , fg : Maybe Ansi.Color
    , bold : Bool
    , underline : Bool
    , inverted : Bool
    , faint : Bool
    , italic : Bool
    , blink : Bool
    , framed : Bool
    , fraktur : Bool
    }


defaultAnsiState : AnsiState
defaultAnsiState =
    { bg = Nothing
    , fg = Nothing
    , bold = False
    , underline = False
    , inverted = False
    , faint = False
    , italic = False
    , blink = False
    , framed = False
    , fraktur = False
    }



-- UPDATE


updateAnsiState : Ansi.Action -> AnsiState -> AnsiState
updateAnsiState action state =
    case action of
        Ansi.SetForeground color ->
            { state | fg = color }

        Ansi.SetBackground color ->
            { state | bg = color }

        Ansi.SetBold value ->
            { state | bold = value }

        Ansi.SetUnderline value ->
            { state | underline = value }

        Ansi.SetItalic value ->
            { state | italic = value }

        _ ->
            state


type alias MessageResult =
    { message : Message
    , state : AnsiState
    , remainder : String
    }


messageFromText : AnsiState -> String -> MessageResult
messageFromText initState message =
    let
        actions =
            Ansi.parse message

        ( finalState, finalChunks, finalRemainder ) =
            List.foldl
                (\action ( state, chunks, remainder ) ->
                    let
                        newState =
                            updateAnsiState action state

                        maybeChunk =
                            case action of
                                Ansi.Linebreak ->
                                    Just AnsiLinebreak

                                Ansi.Print text ->
                                    Just (AnsiPrint newState text)

                                _ ->
                                    Nothing

                        newRemainder =
                            case action of
                                Ansi.Remainder text ->
                                    text

                                _ ->
                                    remainder
                    in
                    ( newState
                    , case maybeChunk of
                        Nothing ->
                            chunks

                        Just chunk ->
                            chunks ++ [ chunk ]
                    , newRemainder
                    )
                )
                ( initState, [], "" )
                actions

        finalMessage =
            { text = message
            , chunks = finalChunks
            }
    in
    { message = finalMessage
    , state = finalState
    , remainder = finalRemainder
    }


type Msg
    = NoOp
    | DraftChanged String
    | HostChanged String
    | PortChanged (Maybe Int)
    | ServerChanged String (Maybe Int)
    | ProxyChanged String
    | ToggleProxy
    | ColorSchemeToggled
    | DownloadMessages
      -- Websocket
    | Send
    | Recv String
    | WebsocketStateChange ConnectionState
    | Connect Server
    | Disconnect
      -- Pages
    | ShowPage (Maybe Page.Page)
    | AliasMsg Page.Alias.Msg


{-| Convert message into plaintext (just text and linebreaks).

Used to generate the log download file.

-}
encodeMessage : Message -> String
encodeMessage message =
    List.foldl
        (\chunk string ->
            case chunk of
                AnsiLinebreak ->
                    string ++ "\n"

                AnsiPrint _ text ->
                    string ++ text
        )
        ""
        message.chunks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowPage page ->
            ( { model | page = page }
            , Cmd.none
            )

        AliasMsg subMsg ->
            case model.page of
                Just (Page.AliasPage subModel) ->
                    let
                        ( pageModel, subCmd, msgFromPage ) =
                            Page.Alias.update subMsg subModel

                        ( newModel, extraCmd ) =
                            case msgFromPage of
                                Page.Alias.NoOp ->
                                    ( { model | page = Just (Page.AliasPage pageModel) }, Cmd.none )

                                Page.Alias.SetAliases newAliases ->
                                    ( { model | aliases = newAliases, page = Nothing }
                                    , Cmd.none
                                    )

                                Page.Alias.Exit ->
                                    ( { model | page = Nothing }, Cmd.none )
                    in
                    ( newModel
                    , Cmd.batch [ Cmd.map AliasMsg subCmd, extraCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        ProxyChanged text ->
            ( { model | proxy = text }
            , Cmd.none
            )

        ToggleProxy ->
            let
                proxy =
                    if model.proxy == prodProxy then
                        devProxy

                    else
                        prodProxy
            in
            ( { model | proxy = proxy }
            , Cmd.none
            )

        DownloadMessages ->
            let
                text =
                    List.foldl
                        (\message string -> string ++ encodeMessage message)
                        ""
                        model.messages
            in
            ( model, downloadMessages ( model.server.host, text ) )

        ColorSchemeToggled ->
            ( { model | isDarkMode = not model.isDarkMode }
            , Cmd.none
            )

        ServerChanged host maybePort ->
            let
                server =
                    model.server

                newServer =
                    { server | host = host, port_ = maybePort }
            in
            ( { model | server = newServer }, Cmd.none )

        Connect server ->
            let
                -- Only clear messages if connecting to a different server.
                -- If we're just reconnecting, we want to see prev messages.
                messages =
                    if model.server == server then
                        model.messages

                    else
                        []
            in
            ( { model
                | server = server
                , messages = messages
              }
            , connect ( server, model.proxy )
            )

        Disconnect ->
            ( model, disconnect () )

        DraftChanged text ->
            ( { model
                | draft = text
              }
            , Cmd.none
            )

        HostChanged host ->
            let
                server =
                    model.server

                newServer =
                    { server | host = host }
            in
            ( { model | server = newServer }
            , Cmd.none
            )

        PortChanged maybePort ->
            let
                server =
                    model.server

                newServer =
                    { server | port_ = maybePort }
            in
            ( { model | server = newServer }
            , Cmd.none
            )

        Send ->
            if model.connectionState /= Connected then
                ( model, Cmd.none )

            else
                let
                    trimmedDraft =
                        String.trim model.draft

                    -- Check for alias match
                    commands =
                        case Util.some (\alias_ -> Alias.apply alias_ trimmedDraft) model.aliases of
                            Nothing ->
                                -- Not an alias
                                [ trimmedDraft ]

                            Just v ->
                                -- Is an alias
                                v
                                    |> String.lines
                                    |> List.map String.trim

                    echoMessages =
                        let
                            style =
                                { defaultAnsiState | fg = Just Ansi.Cyan }
                        in
                        commands
                            |> List.indexedMap
                                (\i cmd ->
                                    let
                                        -- Only trail a newline on final command to avoid \n\n.
                                        suffix =
                                            if i == List.length commands - 1 then
                                                "\n"

                                            else
                                                ""
                                    in
                                    messageFromText style ("\n> " ++ cmd ++ suffix)
                                        |> .message
                                )
                in
                ( { model
                    | messages = model.messages ++ echoMessages
                  }
                , if model.connectionState == Connected then
                    -- Proxy expects us to append \r\n
                    Cmd.batch
                        (List.map
                            (\cmd -> sendMessage (String.trim cmd ++ "\u{000D}\n"))
                            commands
                        )

                  else
                    Cmd.none
                )

        Recv message ->
            let
                result =
                    messageFromText model.ansiState (model.remainder ++ message)
            in
            ( { model
                | messages = model.messages ++ [ result.message ]
                , ansiState = result.state
                , remainder = result.remainder
              }
            , Cmd.none
            )

        WebsocketStateChange newState ->
            ( { model
                | connectionState = newState
                , messages =
                    case newState of
                        Disconnected ->
                            let
                                style =
                                    { defaultAnsiState | fg = Just Ansi.Red }
                            in
                            model.messages ++ [ messageFromText style "\nDisconnected.\n" |> .message ]

                        _ ->
                            model.messages
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver Recv
        , websocketStateReceiver
            (\string ->
                case string of
                    "connecting" ->
                        WebsocketStateChange Connecting

                    "connected" ->
                        WebsocketStateChange Connected

                    "disconnected" ->
                        WebsocketStateChange Disconnected

                    _ ->
                        WebsocketStateChange Disconnected
            )
        ]



-- VIEW


{-| Generate html styles for the <span> that wraps messages.

    ansiStyles { fg = Red, bold = True, ... } == [ class "fg-red", class "bold" ]

-}
ansiStyles : AnsiState -> List (Attribute msg)
ansiStyles state =
    let
        colorStyles : Ansi.Color -> Bool -> List (Attribute msg)
        colorStyles color isForeground =
            let
                prefix =
                    if isForeground then
                        "fg-"

                    else
                        "bg-"

                brightClass =
                    prefix ++ "bright"
            in
            case color of
                Ansi.White ->
                    [ class (prefix ++ "white") ]

                Ansi.Black ->
                    [ class (prefix ++ "black") ]

                Ansi.Red ->
                    [ class (prefix ++ "red") ]

                Ansi.Green ->
                    [ class (prefix ++ "green") ]

                Ansi.Yellow ->
                    [ class (prefix ++ "yellow") ]

                Ansi.Blue ->
                    [ class (prefix ++ "blue") ]

                Ansi.Magenta ->
                    [ class (prefix ++ "magenta") ]

                Ansi.Cyan ->
                    [ class (prefix ++ "cyan") ]

                Ansi.BrightWhite ->
                    class brightClass :: colorStyles Ansi.White isForeground

                Ansi.BrightBlack ->
                    class brightClass :: colorStyles Ansi.Black isForeground

                Ansi.BrightRed ->
                    class brightClass :: colorStyles Ansi.Red isForeground

                Ansi.BrightGreen ->
                    class brightClass :: colorStyles Ansi.Green isForeground

                Ansi.BrightYellow ->
                    class brightClass :: colorStyles Ansi.Yellow isForeground

                Ansi.BrightBlue ->
                    class brightClass :: colorStyles Ansi.Blue isForeground

                Ansi.BrightMagenta ->
                    class brightClass :: colorStyles Ansi.Magenta isForeground

                Ansi.BrightCyan ->
                    class brightClass :: colorStyles Ansi.Cyan isForeground

                -- Cute idea: Bucket arbitrary colors into a small set of CSS classes
                -- so that we can ensure all colors look good in both dark and light color schemes.
                Ansi.Custom r g b ->
                    let
                        attr =
                            if isForeground then
                                "color"

                            else
                                "background-color"
                    in
                    [ style attr <|
                        "rgb("
                            ++ String.fromInt r
                            ++ ","
                            ++ String.fromInt b
                            ++ ","
                            ++ String.fromInt g
                            ++ ")"
                    ]
    in
    List.concat
        [ case state.fg of
            Nothing ->
                []

            Just color ->
                colorStyles color True
        , case state.bg of
            Nothing ->
                []

            Just color ->
                colorStyles color False
        , if state.bold then
            [ class "bold" ]

          else
            []
        , if state.underline then
            [ class "underline" ]

          else
            []
        , if state.italic then
            [ class "italic" ]

          else
            []
        ]


viewMessage : Message -> List (Html msg)
viewMessage message =
    let
        chunkToElement : Chunk -> Html msg
        chunkToElement chunk =
            case chunk of
                AnsiPrint state text ->
                    span (ansiStyles state) (Linkify.html text)

                AnsiLinebreak ->
                    br [] []

        chunkElements =
            message.chunks
                |> List.foldl
                    (\chunk elements ->
                        elements ++ [ Html.Lazy.lazy chunkToElement chunk ]
                    )
                    []
    in
    chunkElements


viewTop : Model -> Html Msg
viewTop model =
    div [] <|
        List.concat
            [ [ button
                    [ onClick ColorSchemeToggled
                    , class "icon-button"
                    , title "Toggle dark mode"
                    , style "float" "right"
                    ]
                    [ Icon.moon 16 ]
              , button
                    [ onClick DownloadMessages
                    , style "float" "right"
                    , class "icon-button"
                    , title "Download history"
                    , disabled (List.isEmpty model.messages)
                    ]
                    [ Icon.download 16 ]
              ]
            , case model.connectionState of
                Connecting ->
                    [ span [ class "fg-yellow" ] [ text "Connecting... " ] ]

                Connected ->
                    [ span [ class "fg-green" ]
                        [ text
                            ("Connected to "
                                ++ model.server.host
                                ++ ":"
                                ++ (model.server.port_
                                        |> Maybe.map String.fromInt
                                        |> Maybe.withDefault "23"
                                   )
                                ++ " "
                            )
                        ]
                    , button
                        [ onClick Disconnect ]
                        [ text "Disconnect" ]
                    , -- Navigation
                      button
                        [ onClick (ShowPage (Just (Page.AliasPage (Page.Alias.init (List.map (\al -> ( al.matchPattern, al.replacePattern )) model.aliases))))) ]
                        [ text "Aliases" ]
                    ]

                Disconnected ->
                    [ span [ class "fg-red" ] [ text "Not connected " ]
                    , button
                        [ onClick (Connect model.server)
                        , disabled (not (isValidProxyUrl model.proxy))
                        ]
                        [ text "Connect" ]
                    , text " "
                    , input
                        [ value model.server.host
                        , placeholder "Host (e.g. example.com)"
                        , onInput (\host -> ServerChanged host model.server.port_)
                        ]
                        []
                    , input
                        [ value
                            (case model.server.port_ of
                                Nothing ->
                                    ""

                                Just port_ ->
                                    String.fromInt port_
                            )
                        , placeholder "Port (e.g. 23)"
                        , onInput
                            (\value ->
                                ServerChanged model.server.host (String.toInt value)
                            )
                        ]
                        []
                    , div
                        [ style "float" "right"
                        , style "line-height" "24px"
                        , style "margin-right" ".5em"
                        , style "display" "inline-block"
                        ]
                        [ text " "
                        , a [ href "https://github.com/danneu/elm-mudclient" ]
                            [ text "github    " ]
                        ]
                    , div
                        [ style "display" "inline-block"
                        , style "float" "right"
                        , style "margin-right" "1em"
                        ]
                        [ span
                            [ class
                                (if isValidProxyUrl model.proxy then
                                    ""

                                 else
                                    "blink fg-red"
                                )
                            ]
                            [ text "Telnet proxy url: " ]
                        , input
                            [ placeholder "e.g. http://localhost:8080"
                            , onInput ProxyChanged
                            , value model.proxy
                            , onDoubleClick ToggleProxy
                            , style "width" "16rem"
                            ]
                            []
                        ]
                    , div [ class "canned-servers" ]
                        [ p [] [ text "Or try these servers..." ]
                        , ul []
                            [ li [] [ text "English: " ]
                            , li []
                                [ button
                                    [ onClick (Connect (Server "aardmud.org" (Just 23))) ]
                                    [ text "Aardwolf" ]
                                , button
                                    [ onClick (Connect (Server "thresholdrpg.com" (Just 3333))) ]
                                    [ text "Threshold RPG" ]
                                , button
                                    [ onClick (Connect (Server "elephant.org" (Just 23))) ]
                                    [ text "Elephant" ]
                                ]
                            ]
                        , ul []
                            [ li [] [ text "Spanish: " ]
                            , li []
                                [ button
                                    [ onClick (Connect (Server "rlmud.org" (Just 23))) ]
                                    [ text "Reinos de Leyenda" ]
                                ]
                            , li []
                                [ button
                                    [ onClick (Connect (Server "cyberlife.es" (Just 7777))) ]
                                    [ text "Cyberlife" ]
                                ]
                            , li []
                                [ button
                                    [ onClick (Connect (Server "mud.balzhur.org" (Just 5400))) ]
                                    [ text "Balzhur" ]
                                ]
                            , li []
                                [ button
                                    [ onClick (Connect (Server "medinamud.ml" (Just 3232))) ]
                                    [ text "Medina" ]
                                ]

                            -- Simauria is nice but the email verification is broken and
                            -- doesn't let you play after registration. :(
                            -- , li []
                            --     [ button
                            --         [ onClick (Connect (Server "mud.simauria.org" (Just 23))) ]
                            --         [ text "Simauria" ]
                            --     ]
                            ]
                        ]
                    ]
            ]


viewPage : Html Msg -> Html Msg
viewPage content =
    div
        []
        [ div [ class "page-overlay" ] []
        , div [ class "page closer" ]
            [ div [ class "stage" ]
                [ div [ class "modal-container", style "margin" "1rem auto" ]
                    [ div [ class "modal-body" ]
                        [ content ]
                    ]
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "elm-mudclient"
    , body =
        [ div
            [ class "container"
            , class
                (if model.isDarkMode then
                    "dark-mode"

                 else
                    "light-mode"
                )
            ]
            [ case model.page of
                Nothing ->
                    text ""

                Just (Page.AliasPage pageModel) ->
                    -- Page.view (Page.Alias.view pageModel) |> map AliasMsg
                    viewPage (Page.Alias.view pageModel |> Html.map AliasMsg)
            , div [ class "top" ]
                [ viewTop model ]
            , div [ class "mid" ]
                [ pre []
                    -- TODO: Refactor so I can use Html.Lazy
                    (List.concatMap viewMessage model.messages)
                ]
            , div [ class "bot" ]
                [ input
                    [ onInput DraftChanged
                    , on "keydown"
                        (JD.field "key" JD.string
                            |> JD.andThen
                                (\key ->
                                    case key of
                                        "Enter" ->
                                            JD.succeed Send

                                        -- "ArrowUp" ->
                                        --     JD.succeed PutHistory
                                        _ ->
                                            JD.fail "some other key"
                                )
                        )
                    , value model.draft
                    , placeholder "Write a command and enter..."
                    , type_ "text"
                    , autofocus True
                    ]
                    []
                , p
                    []
                    [ case Alias.firstMatch model.draft model.aliases of
                        Nothing ->
                            span [ style "visibility" "hidden" ] [ text "--" ]

                        Just a ->
                            text ("Alias match: \"" ++ a.matchPattern ++ "\"")
                    ]
                ]
            ]
        ]
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
