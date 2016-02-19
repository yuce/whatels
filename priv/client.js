
const path = process.argv[2];
if (!path) {
    console.log('usage: node client.js filename.erl');
    process.exit(1);
}

var whatels = require('./whatels');
var w = whatels.createConnection();
w.open(() => {
    w.getSymbols(path, (err, symbols) => {
        if (err) {
            console.log('ERR ', err);
        }
        else {
            console.log('Symb: ', symbols);
        }
        w.close();
    });
});