// @ts-ignore
import { Elm } from './src/Main.elm'

const app = Elm.Main.init({
    flags: {
        isDarkMode: userPrefersDarkMode(),
        proxy: new URLSearchParams(window.location.search).get('proxy'),
    },
})

// HELPERS

function userPrefersDarkMode() {
    const prefersDarkMode =
        !!window.matchMedia &&
        window.matchMedia('(prefers-color-scheme: dark)').matches
    // FIXME: always return dark mode for now, it is just superior to light mode
    return true
}

function scrollLogToBottom() {
    requestAnimationFrame(() => {
        const $mid = document.querySelector('.mid')
        if ($mid) {
            $mid.scrollTop = $mid.scrollHeight
        }
    })
}

// WEBSOCKET

type ConnState = 'disconnected' | 'connecting' | 'connected'
let connectionState: ConnState = 'disconnected'
let socket: WebSocket | null = null

function handleConnectionState(newConnectionState: ConnState) {
    connectionState = newConnectionState
    app.ports.recvWebsocketState.send(newConnectionState)
}

function onerror(error: Event) {
    console.error('error connecting to telnet proxy:', error)
}

function onopen() {
    console.log('websocket connected')
}

function onmessage(event: MessageEvent) {
    console.log('recv:', event.data)
    // Consider ourselves connecting until we get our first msg from server
    if (connectionState === 'connecting') {
        handleConnectionState('connected')
    }
    app.ports.recvMessage.send(event.data)
    scrollLogToBottom()
}

function onclose() {
    console.log('websocket closed')
    handleConnectionState('disconnected')

    socket?.removeEventListener('error', onerror)
    socket?.removeEventListener('open', onopen)
    socket?.removeEventListener('close', onclose)
    socket?.removeEventListener('message', onmessage)
    socket = null
}

// returns websocket
function connect(server: ServerInfo, proxyUrl: string): WebSocket {
    if (socket) {
        socket.close()
        socket = null
    }

    console.log(
        `connecting to "telnet://${server.host}:${server.port_}" through telnet proxy "${proxyUrl}"`,
    )
    handleConnectionState('connecting')
    const url = `${proxyUrl}?host=${encodeURIComponent(server.host)}&port=${
        server.port_ || 23
    }`

    // named `s` instead of shadowing `socket` to avoid TDZ lol
    const s = new WebSocket(url)
    s.addEventListener('error', onerror)
    s.addEventListener('open', onopen)
    s.addEventListener('message', onmessage)
    s.addEventListener('close', onclose)
    return s
}

// PORTS

type ServerInfo = {
    host: string
    port_: string
}

app.ports.connect.subscribe(([server, proxyUrl]: [ServerInfo, string]) => {
    console.log('elm requesting connection')
    socket = connect(server, proxyUrl)
})

app.ports.disconnect.subscribe(() => {
    console.log('client request websocket disconnect')
    socket?.close()
    scrollLogToBottom()
})

app.ports.sendMessage.subscribe((message: string) => {
    socket?.send(message)
    document.querySelector('input')?.select()
})

// TODO: Push log over port as array of text rather one massive string
app.ports.downloadMessages.subscribe(([mudHost, text]: [string, string]) => {
    if (
        !confirm(
            'Do you want to download the history log?\n\n(Your browser may freeze while the log is prepared for download)',
        )
    ) {
        return
    }
    const filename = `${mudHost}-${new Date().toISOString().slice(0, 10)}.log`

    // NOTE: Can do this in Javascript, but would be more robust in Elm
    // instead of relying on DOM:
    //
    // let text = []
    // for (const node of document.querySelector('.mid pre').childNodes) {
    //   switch (node.nodeName) {
    //     case '#text':
    //       console.log({ text: node.textContent })
    //       text.push(node.textContent)
    //       break
    //     case 'SPAN':
    //       text.push(node.textContent)
    //       break
    //     case 'BR':
    //       text.push('\n')
    //       break
    //   }
    // }

    const blob = new Blob([text], {
        type: 'text/plain',
    })
    const a = document.createElement('a')
    document.body.appendChild(a)
    a.download = filename
    a.href = URL.createObjectURL(blob)
    a.click()
    document.body.removeChild(a)
})
