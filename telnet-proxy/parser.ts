import { Transform } from 'stream'

/*
  Parses text into a sequence of telnet command chunks and data chunks.

  Example:

      net.connect(url)
        .pipe(zlib.createInflate())
        .pipe(Parser.createStream())
        .on('data', (chunk) => {
          switch (chunk.type) {
            case 'DATA':
              console.log('data chunk (%d bytes)', chunk.data.length)
              break
            case 'NEGOTIATION':
              console.log('server is negotiating with us')
              break
            case 'CMD':
              console.log('received other telnet command')
              break
          }
        })
*/

export { Cmd, Dmc, Parser, Chunk }

const Cmd = {
    IAC: 255,
    // Negotiation
    WILL: 251,
    WONT: 252,
    DO: 253,
    DONT: 254,
    // Subnegotiation
    SE: 240,
    SB: 250,
    // General options
    ECHO: 1,
    SUPPRESS_GO_AHEAD: 3,
    STATUS: 5,
    TIMING_MARK: 6,
    EXTENDED_ASCII: 17,
    TERMINAL_SPEED: 24,
    WINDOW_SIZE: 31, // https://www.rfc-editor.org/rfc/rfc1073.html Negotiate about window size (NAWS)
    REMOTE_FLOW_CONTROL: 33,
    LINEMODE: 34,
    ENVIRON: 36,
    NEW_ENVIRON: 39, // https://www.rfc-editor.org/rfc/rfc1572.html
    CHARSET: 42,
    NOP: 241,
    ARE_YOU_THERE: 246,
    GO_AHEAD: 249,
    // MUD options https://mudcoders.fandom.com/wiki/List_of_Telnet_Options
    MSDP: 69,
    MSSP: 70,
    MCCP1: 85,
    MCCP2: 86,
    MCCP3: 87,
    MSP: 90,
    MXP: 91,
    ZMP: 93,
    ATCP: 200,
    GMCP: 201,
}

// Look up friendly code name from a code number
const Dmc = (() => {
    const inverted: { [key: number]: string } = {}
    for (const [k, v] of Object.entries(Cmd)) {
        inverted[v] = k
    }
    return inverted
})()

type Chunk =
    // Non-command data
    | { type: 'DATA'; data: Uint8Array }
    // Negotiation
    | { type: 'NEGOTIATION'; name: 'WILL'; target: number }
    | { type: 'NEGOTIATION'; name: 'WONT'; target: number }
    | { type: 'NEGOTIATION'; name: 'DO'; target: number }
    | { type: 'NEGOTIATION'; name: 'DONT'; target: number }
    | { type: 'NEGOTIATION'; name: 'SB'; target: number; data: Uint8Array }
    // Other commands like IAC AYT, IAC GA, etc.
    | { type: 'CMD'; code: number }

// match(this.buf, [Cmd.IAC, Cmd.DO, 'number'])
// 'number' matches a single number slot
function match(buf: number[], pattern: (number | 'number')[]): boolean {
    if (buf.length < pattern.length) {
        return false
    }
    for (let i = 0; i < pattern.length; i++) {
        if (pattern[i] === 'number' && typeof buf[i] !== 'number') {
            return false
        } else if (pattern[i] !== 'number' && pattern[i] !== buf[i]) {
            return false
        }
    }
    return true
}

class Parser {
    buf: number[] = []

    static createStream(): Transform {
        const parser = new Parser()
        return new Transform({
            objectMode: true,
            // TODO: Custom flush?
            // flush() {},
            transform(data, _, done) {
                parser.push(data)
                let chunk
                while ((chunk = parser.next())) {
                    this.push(chunk)
                }
                done()
            },
        })
    }

    push(bytes: Uint8Array | Buffer) {
        bytes.forEach((b) => {
            this.buf.push(b)
        })
    }

    // decodes next chunk from buf, if possible
    // mutates this.buf
    next(): Chunk | null {
        let i = 0
        let data = []

        // Detect data chunk (consume data bytes from start until IAC)
        while (i < this.buf.length) {
            if (this.buf[i] === Cmd.IAC) {
                break
            }
            data.push(this.buf[i])
            i++
        }

        if (data.length > 0) {
            this.buf.splice(0, data.length)
            return { type: 'DATA', data: Uint8Array.from(data) }
        }

        // Decode IAC chunk
        data = []
        if (match(this.buf, [Cmd.IAC, Cmd.DO, 'number'])) {
            const chunk: Chunk = {
                type: 'NEGOTIATION',
                name: 'DO',
                target: this.buf[2],
            }
            this.buf.splice(0, 3)
            return chunk
        } else if (match(this.buf, [Cmd.IAC, Cmd.DONT, 'number'])) {
            const chunk: Chunk = {
                type: 'NEGOTIATION',
                name: 'DONT',
                target: this.buf[2],
            }
            this.buf.splice(0, 3)
            return chunk
        } else if (match(this.buf, [Cmd.IAC, Cmd.WILL, 'number'])) {
            const chunk: Chunk = {
                type: 'NEGOTIATION',
                name: 'WILL',
                target: this.buf[2],
            }
            this.buf.splice(0, 3)
            return chunk
        } else if (match(this.buf, [Cmd.IAC, Cmd.WONT, 'number'])) {
            const chunk: Chunk = {
                type: 'NEGOTIATION',
                name: 'WONT',
                target: this.buf[2],
            }
            this.buf.splice(0, 3)
            return chunk
        } else if (match(this.buf, [Cmd.IAC, Cmd.SB, 'number'])) {
            // IAC SB <number> <bytes*> IAC SE
            let i = 3
            let data = []
            while (i < this.buf.length) {
                if (this.buf[i] === Cmd.IAC && this.buf[i + 1] === Cmd.SE) {
                    const chunk: Chunk = {
                        type: 'NEGOTIATION',
                        name: 'SB',
                        target: this.buf[2],
                        data: Uint8Array.from(data),
                    }
                    this.buf.splice(0, i + 1 + 1)
                    return chunk
                } else {
                    data.push(this.buf[i])
                }
                i++
            }
        } else if (match(this.buf, [Cmd.IAC, 'number'])) {
            const chunk: Chunk = { type: 'CMD', code: this.buf[1] }
            this.buf.splice(0, 2)
            return chunk
        } else {
            // Not enough buffered data to parse a chunk
        }

        return null
    }
}

// const parser = new Parser()
// parser.push(Buffer.from([
//   Cmd.IAC, Cmd.DO, Cmd.MCCP2,
//   Cmd.IAC, Cmd.SB, 1, 2, 3, Cmd.IAC, Cmd.SE,
//   Cmd.IAC, Cmd.DO, Cmd.MCCP2,
// ]))
// console.log('buf', parser.buf)
// console.log(parser.next())
// console.log('buf', parser.buf)
// console.log(parser.next())
// console.log('buf', parser.buf)
// console.log(parser.next())

// const buffer = Uint8Array.from([
//   1, 2, 3,
//   Cmd.IAC, Cmd.DO, Cmd.MCCP2,
//   Cmd.IAC, Cmd.SB, 1, 2, 3, Cmd.IAC, Cmd.SE,
//   Cmd.IAC, Cmd.DO, Cmd.MCCP2,
//   4, 5, 6,
// ])

// const r = new Readable()
// r.push(buffer)
// r.push(null)

// const stream = Parser.createStream()
// stream.on('data', chunk => {
//   console.log({chunk})
// })
// stream.on('end', () => {
//   console.log('parser end')
// })
// r.pipe(stream)
