"use strict";
exports.__esModule = true;
exports.Parser = exports.Dmc = exports.Cmd = void 0;
var stream_1 = require("stream");
var Cmd = {
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
    WINDOW_SIZE: 31,
    REMOTE_FLOW_CONTROL: 33,
    LINEMODE: 34,
    ENVIRON: 36,
    NEW_ENVIRON: 39,
    CHARSET: 42,
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
    GMCP: 201
};
exports.Cmd = Cmd;
// Look up friendly code name from a code number
var Dmc = (function () {
    var inverted = {};
    for (var _i = 0, _a = Object.entries(Cmd); _i < _a.length; _i++) {
        var _b = _a[_i], k = _b[0], v = _b[1];
        inverted[v] = k;
    }
    return inverted;
})();
exports.Dmc = Dmc;
// match(this.buf, [Cmd.IAC, Cmd.DO, 'number'])
// 'number' matches a single number slot
function match(buf, pattern) {
    if (buf.length < pattern.length) {
        return false;
    }
    for (var i = 0; i < pattern.length; i++) {
        if (pattern[i] === 'number' && typeof buf[i] !== 'number') {
            return false;
        }
        else if (pattern[i] !== 'number' && pattern[i] !== buf[i]) {
            return false;
        }
    }
    return true;
}
var Parser = /** @class */ (function () {
    function Parser() {
        this.buf = [];
    }
    Parser.createStream = function () {
        var parser = new Parser();
        return new stream_1.Transform({
            objectMode: true,
            // TODO: Custom flush?
            // flush() {},
            transform: function (data, _, done) {
                parser.push(data);
                var chunk;
                while ((chunk = parser.next())) {
                    this.push(chunk);
                }
                done();
            }
        });
    };
    Parser.prototype.push = function (bytes) {
        var _this = this;
        bytes.forEach(function (b) {
            _this.buf.push(b);
        });
    };
    // decodes next chunk from buf, if possible
    // mutates this.buf
    Parser.prototype.next = function () {
        var i = 0;
        var data = [];
        // Detect data chunk (consume data bytes from start until IAC)
        while (i < this.buf.length) {
            if (this.buf[i] === Cmd.IAC) {
                break;
            }
            data.push(this.buf[i]);
            i++;
        }
        if (data.length > 0) {
            this.buf.splice(0, data.length);
            return { type: 'DATA', data: Uint8Array.from(data) };
        }
        // Decode IAC chunk
        data = [];
        if (match(this.buf, [Cmd.IAC, Cmd.DO, 'number'])) {
            var chunk = {
                type: 'NEGOTIATION',
                name: 'DO',
                target: this.buf[2]
            };
            this.buf.splice(0, 3);
            return chunk;
        }
        else if (match(this.buf, [Cmd.IAC, Cmd.DONT, 'number'])) {
            var chunk = {
                type: 'NEGOTIATION',
                name: 'DONT',
                target: this.buf[2]
            };
            this.buf.splice(0, 3);
            return chunk;
        }
        else if (match(this.buf, [Cmd.IAC, Cmd.WILL, 'number'])) {
            var chunk = {
                type: 'NEGOTIATION',
                name: 'WILL',
                target: this.buf[2]
            };
            this.buf.splice(0, 3);
            return chunk;
        }
        else if (match(this.buf, [Cmd.IAC, Cmd.WONT, 'number'])) {
            var chunk = {
                type: 'NEGOTIATION',
                name: 'WONT',
                target: this.buf[2]
            };
            this.buf.splice(0, 3);
            return chunk;
        }
        else if (match(this.buf, [Cmd.IAC, Cmd.SB, 'number'])) {
            // IAC SB <number> <bytes*> IAC SE
            var i_1 = 3;
            var data_1 = [];
            while (i_1 < this.buf.length) {
                if (this.buf[i_1] === Cmd.IAC && this.buf[i_1 + 1] === Cmd.SE) {
                    var chunk = {
                        type: 'NEGOTIATION',
                        name: 'SB',
                        target: this.buf[2],
                        data: Uint8Array.from(data_1)
                    };
                    this.buf.splice(0, i_1 + 1 + 1);
                    return chunk;
                }
                else {
                    data_1.push(this.buf[i_1]);
                }
                i_1++;
            }
        }
        else if (match(this.buf, [Cmd.IAC, 'number'])) {
            var chunk = { type: 'CMD', code: this.buf[1] };
            this.buf.splice(0, 2);
            return chunk;
        }
        else {
            // Not enough buffered data to parse a chunk
        }
        return null;
    };
    return Parser;
}());
exports.Parser = Parser;
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
