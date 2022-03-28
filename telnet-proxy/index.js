"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
exports.__esModule = true;
var ws = require("ws");
var net = require("net");
var zlib = require("zlib");
var parser_1 = require("./parser");
// https://users.cs.cf.ac.uk/Dave.Marshall/Internet/node141.html
var PORT = Number.parseInt(process.env.PORT || '', 10) || 8080;
var server = new ws.WebSocketServer({
    port: PORT
});
server.on('connection', function (socket, req) {
    var _a = (function () {
        var url = new URL(req.url, 'ws://localhost');
        return [
            url.searchParams.get('host'),
            url.searchParams.get('port')
                ? Number.parseInt(url.searchParams.get('port'), 10)
                : 23,
            url.searchParams.get('mccp2') === 'true',
        ];
    })(), telnetHost = _a[0], telnetPort = _a[1];
    if (!telnetHost) {
        socket.send('Error: Must provide telnet host');
        socket.close();
    }
    console.log('client connected', { telnetHost: telnetHost, telnetPort: telnetPort });
    var charset = { encoding: 'ISO-8859-1', confidence: 0 };
    var telnet = net.connect(telnetPort, telnetHost, function () {
        console.log('ere');
    });
    var parserStream = parser_1.Parser.createStream();
    parserStream.on('data', ondata);
    // Our initial pipeline that may be unpiped and repiped later.
    telnet.pipe(parserStream);
    function prettyChunk(chunk) {
        if ('target' in chunk && chunk.target) {
            return __assign(__assign({}, chunk), { targetName: parser_1.Dmc[chunk.target] || '<unknown>' });
        }
        if ('code' in chunk && chunk.code) {
            return __assign(__assign({}, chunk), { codeName: parser_1.Dmc[chunk.code] || '<unknown>' });
        }
        return chunk;
    }
    function ondata(chunk) {
        console.log('[ondata] recv chunk', prettyChunk(chunk));
        if (chunk.type === 'DATA') {
            console.log('last data:', chunk.data.slice(chunk.data.length - 5));
        }
        // Negotiate MCCP2
        if (chunk.type === 'NEGOTIATION' &&
            chunk.name === 'SB' &&
            chunk.target === parser_1.Cmd.MCCP2) {
            console.log('server sent IAC SB MCCP2 IAC SE. setting up new pipeline...');
            // Re-pipe (telnet -> parser) into (telnet -> decompress -> parser)
            // TODO: Should I flush the parser stream?
            telnet
                .unpipe()
                .pipe(zlib.createInflate({
                // Avoids crashing when partial data is flushed
                finishFlush: zlib.constants.Z_SYNC_FLUSH
            }))
                .pipe(parserStream);
            return;
        }
        switch (chunk.type) {
            case 'DATA':
                var string = new TextDecoder(charset.encoding).decode(chunk.data);
                socket.send(string);
                return;
            case 'CMD':
                switch (chunk.code) {
                    case parser_1.Cmd.ARE_YOU_THERE:
                        telnet.write('Present\r\n');
                        return;
                    default:
                        console.log("unhandled CMD code: ".concat(chunk.code));
                        return;
                }
            case 'NEGOTIATION':
                switch (chunk.target) {
                    case parser_1.Cmd.TERMINAL_SPEED:
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DONT TERMINAL_SPEED to server');
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DONT, parser_1.Cmd.TERMINAL_SPEED]));
                        }
                        return;
                    case parser_1.Cmd.WINDOW_SIZE:
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
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DONT NAWS to server');
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DONT, parser_1.Cmd.WINDOW_SIZE]));
                        }
                        return;
                    case parser_1.Cmd.NEW_ENVIRON:
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DONT NEW_ENVIRON to server');
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DONT, parser_1.Cmd.NEW_ENVIRON]));
                        }
                        return;
                    case parser_1.Cmd.ECHO:
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DONT ECHO to server');
                            // telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.ECHO]))
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DO, parser_1.Cmd.ECHO]));
                        }
                        return;
                    case parser_1.Cmd.MCCP2:
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DO MCCP2 to server');
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DO, parser_1.Cmd.MCCP2]));
                            // console.log('sending IAC DONT MCCP2 to server')
                            // telnet.write(Uint8Array.from([Cmd.IAC, Cmd.DONT, Cmd.MCCP2]))
                        }
                        return;
                    case parser_1.Cmd.GMCP:
                        if (chunk.target === parser_1.Cmd.WILL) {
                            console.log('sending IAC DO GMCP to server');
                            telnet.write(Uint8Array.from([parser_1.Cmd.IAC, parser_1.Cmd.DO, parser_1.Cmd.GMCP]));
                        }
                        return;
                    default:
                        console.log('unhandled negotation:', chunk);
                        return;
                }
        }
    }
    telnet.on('error', function (error) {
        console.log('telnet error', error);
    });
    telnet.on('close', function () {
        console.log('telnet close');
        socket.close();
    });
    telnet.on('end', function () {
        console.log('telnet end');
        socket.close();
    });
    socket.on('message', function (message, isBinary) {
        console.log("[binary=".concat(isBinary, "] recv: %s"), message);
        var bytes = new TextEncoder().encode(message.toString());
        telnet.write(bytes);
    });
    socket.on('close', function () {
        console.log('client websocket closed');
        telnet.end();
    });
});
console.log("listening on  ".concat(PORT, "..."));
