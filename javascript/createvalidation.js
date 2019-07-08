const fs = require('fs');

if (process.argv.length !== 3) {
    console.log("Missing filename");
    console.log(" Usage: node createvalidation.js filename");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

console.log("const importObject = { imports: { } };");
console.log("const buffer = new Uint8Array(\[", b.toString(), "]);");
//console.log("let buffer = new Uint8Array(\[ ]);");
//console.log("throw new Error('validation failed');"); //injected error to test setup
console.log("print(WebAssembly.validate(buffer));");
//console.log("if (!(WebAssembly.validate(buffer))) { throw new Error('validation failed'); }");
