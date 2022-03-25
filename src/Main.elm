port module Main exposing (..)

import Ansi
import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Icon
import Json.Decode as JD
import Regex



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS
-- Elm -> JS


{-| Sends (MUD server, telnet proxy url) to JS.
-}
port connect : ( Server, String ) -> Cmd msg


port disconnect : () -> Cmd msg


port downloadMessages : ( String, String ) -> Cmd msg


port sendMessage : String -> Cmd msg



-- JS -> Elm


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


type alias Model =
    { draft : String
    , ansiState : AnsiState
    , remainder : String
    , messages : List Message
    , connectionState : ConnectionState
    , server : Server
    , proxy : String
    , isDarkMode : Bool
    }


type alias Flags =
    { isDarkMode : Bool }


defaultFlags : Flags
defaultFlags =
    { isDarkMode = True }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { draft = ""
      , ansiState = defaultAnsiState
      , remainder = ""
      , messages = []
      , connectionState = Disconnected
      , isDarkMode = flags.isDarkMode
      , server = Server "elephant.org" (Just 23)

      --   , proxy = "ws://localhost:8080"
      , proxy = "wss://telnet-proxy.fly.dev"
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
    | ColorSchemeToggled
    | DownloadMessages
      -- Websocket
    | Send
    | Recv String
    | WebsocketStateChange ConnectionState
    | Connect Server
    | Disconnect


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

        ProxyChanged text ->
            ( { model | proxy = text }
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
                    echoMessage =
                        messageFromText { defaultAnsiState | fg = Just Ansi.Cyan } ("\n> " ++ model.draft ++ "\n")
                            |> .message
                in
                ( { model
                    | messages = model.messages ++ [ echoMessage ]
                  }
                , if model.connectionState == Connected then
                    -- Proxy expects us to append \r\n
                    sendMessage (model.draft ++ "\u{000D}\n")

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
ansiStyles : AnsiState -> List (H.Attribute msg)
ansiStyles state =
    let
        colorStyles : Ansi.Color -> Bool -> List (H.Attribute msg)
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
                    [ HA.class (prefix ++ "white") ]

                Ansi.Black ->
                    [ HA.class (prefix ++ "black") ]

                Ansi.Red ->
                    [ HA.class (prefix ++ "red") ]

                Ansi.Green ->
                    [ HA.class (prefix ++ "green") ]

                Ansi.Yellow ->
                    [ HA.class (prefix ++ "yellow") ]

                Ansi.Blue ->
                    [ HA.class (prefix ++ "blue") ]

                Ansi.Magenta ->
                    [ HA.class (prefix ++ "magenta") ]

                Ansi.Cyan ->
                    [ HA.class (prefix ++ "cyan") ]

                Ansi.BrightWhite ->
                    HA.class brightClass :: colorStyles Ansi.White isForeground

                Ansi.BrightBlack ->
                    HA.class brightClass :: colorStyles Ansi.Black isForeground

                Ansi.BrightRed ->
                    HA.class brightClass :: colorStyles Ansi.Red isForeground

                Ansi.BrightGreen ->
                    HA.class brightClass :: colorStyles Ansi.Green isForeground

                Ansi.BrightYellow ->
                    HA.class brightClass :: colorStyles Ansi.Yellow isForeground

                Ansi.BrightBlue ->
                    HA.class brightClass :: colorStyles Ansi.Blue isForeground

                Ansi.BrightMagenta ->
                    HA.class brightClass :: colorStyles Ansi.Magenta isForeground

                Ansi.BrightCyan ->
                    HA.class brightClass :: colorStyles Ansi.Cyan isForeground

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
                    [ HA.style attr <|
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
            [ HA.class "bold" ]

          else
            []
        , if state.underline then
            [ HA.class "underline" ]

          else
            []
        , if state.italic then
            [ HA.class "italic" ]

          else
            []
        ]


viewMessage : Message -> List (H.Html msg)
viewMessage message =
    let
        chunkToElement : Chunk -> H.Html msg
        chunkToElement chunk =
            case chunk of
                AnsiPrint state text ->
                    let
                        styles =
                            ansiStyles state
                    in
                    if List.isEmpty styles then
                        H.text text

                    else
                        H.span styles [ H.text text ]

                AnsiLinebreak ->
                    H.br [] []

        chunkElements =
            message.chunks
                |> List.foldl
                    (\chunk elements ->
                        elements ++ [ Html.Lazy.lazy chunkToElement chunk ]
                    )
                    []
    in
    chunkElements


viewTop : Model -> H.Html Msg
viewTop model =
    H.div [] <|
        List.concat
            [ [ H.button
                    [ HE.onClick ColorSchemeToggled
                    , HA.class "icon-button"
                    , HA.title "Toggle dark mode"
                    , HA.style "float" "right"
                    ]
                    [ Icon.moonSvg ]
              , H.button
                    [ HE.onClick DownloadMessages
                    , HA.style "float" "right"
                    , HA.class "icon-button"
                    , HA.title "Download history"
                    , HA.disabled (List.isEmpty model.messages)
                    ]
                    [ Icon.downloadSvg ]
              ]
            , case model.connectionState of
                Connecting ->
                    [ H.span [ HA.class "fg-yellow" ] [ H.text "Connecting... " ] ]

                Connected ->
                    [ H.span [ HA.class "fg-green" ]
                        [ H.text
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
                    , H.button
                        [ HE.onClick Disconnect ]
                        [ H.text "Disconnect" ]
                    ]

                Disconnected ->
                    [ H.span [ HA.class "fg-red" ] [ H.text "Not connected " ]
                    , H.button
                        [ HE.onClick (Connect model.server)
                        , HA.disabled (not (isValidProxyUrl model.proxy))
                        ]
                        [ H.text "Connect" ]
                    , H.text " "
                    , H.input
                        [ HA.value model.server.host
                        , HA.placeholder "Host (e.g. example.com)"
                        , HE.onInput (\host -> ServerChanged host model.server.port_)
                        ]
                        []
                    , H.input
                        [ HA.value
                            (case model.server.port_ of
                                Nothing ->
                                    ""

                                Just port_ ->
                                    String.fromInt port_
                            )
                        , HA.placeholder "Port (e.g. 23)"
                        , HE.onInput
                            (\value ->
                                ServerChanged model.server.host (String.toInt value)
                            )
                        ]
                        []
                    , H.div
                        [ HA.style "display" "inline-block"
                        , HA.style "float" "right"
                        , HA.style "margin-right" "1em"
                        ]
                        [ H.span
                            [ HA.class
                                (if isValidProxyUrl model.proxy then
                                    ""

                                 else
                                    "blink fg-red"
                                )
                            ]
                            [ H.text "Telnet proxy url: " ]
                        , H.input
                            [ HA.placeholder "e.g. http://localhost:8080"
                            , HE.onInput ProxyChanged
                            , HA.value model.proxy
                            ]
                            []
                        ]
                    , H.div [ HA.class "canned-servers" ]
                        [ H.p [] [ H.text "Or try these servers..." ]
                        , H.ul []
                            [ H.li [] [ H.text "English: " ]
                            , H.li []
                                [ H.button
                                    [ HE.onClick (Connect (Server "aardmud.org" (Just 23))) ]
                                    [ H.text "Aardwolf" ]
                                , H.button
                                    [ HE.onClick (Connect (Server "thresholdrpg.com" (Just 3333))) ]
                                    [ H.text "Threshold RPG" ]
                                , H.button
                                    [ HE.onClick (Connect (Server "elephant.org" (Just 23))) ]
                                    [ H.text "Elephant" ]
                                ]
                            ]
                        , H.ul []
                            [ H.li [] [ H.text "Spanish: " ]
                            , H.li []
                                [ H.button
                                    [ HE.onClick (Connect (Server "rlmud.org" (Just 23))) ]
                                    [ H.text "Reinos de Leyenda" ]
                                ]
                            , H.li []
                                [ H.button
                                    [ HE.onClick (Connect (Server "cyberlife.es" (Just 7777))) ]
                                    [ H.text "Cyberlife" ]
                                ]
                            , H.li []
                                [ H.button
                                    [ HE.onClick (Connect (Server "mud.balzhur.org" (Just 5400))) ]
                                    [ H.text "Balzhur" ]
                                ]
                            , H.li []
                                [ H.button
                                    [ HE.onClick (Connect (Server "medinamud.ml" (Just 3232))) ]
                                    [ H.text "Medina" ]
                                ]

                            -- Simauria is nice but the email verification is broken and
                            -- doesn't let you play after registration. :(
                            -- , H.li []
                            --     [ H.button
                            --         [ HE.onClick (Connect (Server "mud.simauria.org" (Just 23))) ]
                            --         [ H.text "Simauria" ]
                            --     ]
                            ]
                        ]
                    ]
            ]


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class "container"
        , HA.class
            (if model.isDarkMode then
                "dark-mode"

             else
                "light-mode"
            )
        ]
        [ H.div [ HA.class "top" ]
            [ viewTop model ]
        , H.div [ HA.class "mid" ]
            [ H.pre []
                -- TODO: Refactor so I can use Html.Lazy
                (List.concatMap viewMessage model.messages)
            ]
        , H.div [ HA.class "bot" ]
            [ H.input
                [ HE.onInput DraftChanged
                , HE.on "keydown"
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
                , HA.value model.draft
                , HA.placeholder "Write a command and enter..."
                , HA.type_ "text"
                , HA.autofocus True
                ]
                []
            ]
        ]
