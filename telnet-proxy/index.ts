import * as ws from 'ws'
import * as net from 'net'
import * as zlib from 'zlib'
import { Parser, Chunk, Cmd, Dmc } from './parser'
import { IncomingMessage } from 'http'

// https://users.cs.cf.ac.uk/Dave.Marshall/Internet/node141.html

const PORT = Number.parseInt(process.env.PORT || '', 10) || 8080

const server = new ws.WebSocketServer({
  port: PORT,
})

server.on('connection', (socket: ws.WebSocket, req: IncomingMessage) => {
  const [telnetHost, telnetPort] = (() => {
    const url = new URL(req.url!, 'ws://localhost')
    return [
      url.searchParams.get('host')!,
      url.searchParams.get('port')
        ? Number.parseInt(url.searchParams.get('port')!, 10)
        : 23,
      url.searchParams.get('mccp2') === 'true',
    ]
  })()

  if (!telnetHost) {
    socket.send('Error: Must provide telnet host')
    socket.close()
  }

  console.log('client connected', { telnetHost, telnetPort })
  let charset = { encoding: 'ISO-8859-1', confidence: 0 }

  const telnet = net.connect(telnetPort, telnetHost, () => {
    console.log('ere')
  })

  const parserStream = Parser.createStream()
  parserStream.on('data', ondata)

  // Our initial pipeline that may be unpiped and repiped later.
  telnet.pipe(parserStream)

  function prettyChunk (
    chunk: Chunk,
  ): Chunk & { targetName?: string | undefined } {
    if ('target' in chunk && chunk.target) {
      return { ...chunk, targetName: Dmc[chunk.target] }
    }
    return chunk
  }

  function ondata (chunk: Chunk) {
    console.log('[ondata] recv chunk', prettyChunk(chunk))

    // Negotiate MCCP2
    if (
      chunk.type === 'NEGOTIATION' &&
      chunk.name === 'SB' &&
      chunk.target === Cmd.MCCP2
    ) {
      console.log('server sent IAC SB MCCP2 IAC SE. setting up new pipeline...')

      // Re-pipe (telnet -> parser) into (telnet -> decompress -> parser)
      telnet
        .unpipe()
        .pipe(
          zlib.createInflate({
            // Avoids crashing when partial data is flushed
            finishFlush: zlib.constants.Z_SYNC_FLUSH,
          }),
        )
        .pipe(parserStream)
      return
    }

    switch (chunk.type) {
      case 'DATA':
        const string = new TextDecoder(charset.encoding).decode(chunk.data)
        socket.send(string)
        return
      case 'NEGOTIATION':
        switch (chunk.target) {
          case Cmd.TERMINAL_SPEED:
            console.log('sending IAC DONT TERMINAL_SPEED to server')
            telnet.write(
              Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.TERMINAL_SPEED]),
            )
            return
          case Cmd.WINDOW_SIZE:
            // Here's how we could negotiate window size:
            //
            // const windowCharWidth = 80
            // const windowCharHeight = 0
            // const dvWidth = new ArrayBuffer(2)
            // new DataView(dvWidth).setInt16(0, windowCharWidth, false)
            // const dvHeight = new ArrayBuffer(2)
            // new DataView(dvHeight).setInt16(0, windowCharHeight, false)
            //
            // const bytes = Uint8Array.from([
            //   Cmd.IAC,
            //   Cmd.SB,
            //   ...new Uint8Array(dvWidth),
            //   ...new Uint8Array(dvHeight),
            //   Cmd.IAC,
            //   Cmd.SE,
            // ])
            // telnet.write(bytes)
            // return

            console.log('sending IAC DONT NAWS to server')
            telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.WINDOW_SIZE]))
            return
          case Cmd.NEW_ENVIRON:
            console.log('sending IAC DONT NEW_ENVIRON to server')
            telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.NEW_ENVIRON]))
            return
          case Cmd.ECHO:
            console.log('sending IAC DONT ECHO to server')
            telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.ECHO]))
            return
          case Cmd.MCCP2:
            console.log('sending IAC DO MCCP2 to server')
            telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DO, Cmd.MCCP2]))
            return
          case Cmd.GMCP:
            console.log('sending IAC DO GMCP to server')
            telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DO, Cmd.GMCP]))
            return
          default:
            console.log('unhandled negotation:', chunk)
            return
        }
    }
  }

  telnet.on('error', error => {
    console.log('telnet error', error)
  })

  telnet.on('close', () => {
    console.log('telnet close')
    socket.close()
  })

  telnet.on('end', () => {
    console.log('telnet end')
    socket.close()
  })

  socket.on('message', (message, isBinary) => {
    console.log(`[binary=${isBinary}] recv: %s`, message)

    const bytes = new TextEncoder().encode(message.toString())
    telnet.write(bytes)
  })

  socket.on('close', () => {
    console.log('client websocket closed')
    telnet.end()
  })
})

console.log(`listening on  ${PORT}...`)
