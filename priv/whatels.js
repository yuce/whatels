const net = require('net');
const fs = require('fs');

function createConnection(port, host) {
    const symbolStore = {};
    var socket = null;
    host = host || '127.0.0.1';
    port = port || 10999;

    function connect(callback) {
        socket = net.connect({port: port, host: host}, () => {
            socket.setEncoding('utf8');
            return callback();
        });
    }

    function sendGetSymbolsMessage(path, callback) {
        socket.write(makeGetSymbolsMessage(path));
        socket.on('data', data => {
            const msg = parseText(data);
            if (msg === null) {
                callback('parse_error', '');
            }
            else {
                callback(null, msg.symbols);
            }
        });
    }

    function destroy() {
        socket.destroy();
        socket = null;
    }

    return {
        open: connect,
        getSymbols: sendGetSymbolsMessage,
        close: destroy
    }
}

function parseText(text) {
    try {
        const msg = extractFlipFlop(text);
        return parseMessage(msg);
    }
    catch (Ex) {
        console.log('ex:', Ex);
        return null;
    }
}

function extractFlipFlop(text) {
    const flipIndex = text.indexOf("\r\n");
    if (flipIndex <= 0) {
        return {remaining: text};
    }
    const flip = parseFlip(text.substring(0, flipIndex));
    const endOfPayload = flipIndex + 2 + flip.payloadLen + 2;
    if (text.length < endOfPayload) {
        return {remaining: text};
    }
    const payload = text.substr(flipIndex + 2, flip.payloadLen);
    return {op: flip.op,
            payload: payload,
            remaining: text.substring(endOfPayload)};
}

function parseFlip(flip) {
    const parts = flip.split(" ", 2);
    if (parts.length != 2) {
        throw "parse_error";
    }
    const op = parts[0];
    const payloadLen = parseInt(parts[1]);
    if (isNaN(payloadLen)) {
        throw "parse_error";
    }
    return {op: op, payloadLen: payloadLen};
}

function parseMessage(msg) {
    switch (msg.op) {
        case "SYMBOLS":
            return {op: msg.op,
                    symbols: transformSymbols(JSON.parse(msg.payload))};
        default:
            return null;
    }
}

function transformSymbols(symbols) {
	const newSymbols = {};
    newSymbols.functions = (symbols.functions || []).map(f => {
        return {name: f.name + '/' + f.arity,
                line: f.line};
    });
    return newSymbols;
}

function makeMessage(op, payload) {
    return [op, ' ', payload.length, '\r\n', payload, '\r\n'].join('');
}

function makeGetSymbolsMessage(path) {
    return makeMessage('SYMBOLS?', path);
}

module.exports = {
    createConnection: createConnection
};