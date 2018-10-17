const fs = require('fs');

if (process.argv.length !== 3) {
    console.log("missing filename argument");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

const program = "var buffer = new Uint8Array(\["
    + b.toString()
    + "]);\n"
    + "let m = new WebAssembly.Instance(new WebAssembly.Module(buffer));\n"
    + "console.log(m.exports.aexp());\n"

console.log(program);