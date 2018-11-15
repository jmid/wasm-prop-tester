const fs = require('fs');

if (process.argv.length !== 3) {
    console.log("missing filename argument");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

//"console = console || { log: (...args) => debug(Array.prototype.slice.call(args).join(' ')) };\n"

const program = 
    "var zero_div_re = /(divi.*zero)|(zero.*divi)/i;"
    + "var debug = debug || (arg => console.log('-->', arg));"
    + "var buffer = new Uint8Array(\["
    + b.toString()
    + "]);\n"
    + "let m = new WebAssembly.Instance(new WebAssembly.Module(buffer));\n"
    + "try {\n"
    + "     debug(m.exports.aexp());\n"
    + "} catch(e) {\n"
    + "     if (zero_div_re.exec(e.message)) debug('integer divide by zero')\n"
    + "}"

console.log(program);