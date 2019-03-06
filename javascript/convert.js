const fs = require('fs');

if (process.argv.length !== 3) {
    console.log("missing filename argument");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

//"console = console || { log: (...args) => debug(Array.prototype.slice.call(args).join(' ')) };\n"

const program = 
    "var zero_div_re = /(divi.*zero)|(zero.*divi)/i;\n"
    + "var zero_rem_re = /(rem.*zero)|(zero.*rem)/i;\n"
    + "var overflow_re = /(overflow)/i;\n"
    + "var out_of_bounds_re = /(out.of.bounds)/i;\n"
    + "var unrepresentable_re = /(unrepresentable)/i;\n"
    + "var debug = debug || (arg => console.log('-->', arg));\n"
    + "var importObject = { imports: { log: function(arg) { debug(arg); } } };\n"
    + "var buffer = new Uint8Array(\["
    + b.toString()
    + "]);\n"
    + "try {\n"
    + "     let m = new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject);\n"
    + "     debug(m.exports.aexp(1,2));\n"
    + "} catch(e) {\n"
    + "     //if (zero_div_re.exec(e.message)) debug('integer divide by zero')\n"
    + "     //else if (zero_rem_re.exec(e.message)) debug('integer divide by zero')\n"
    + "     //else if (overflow_re.exec(e.message)) debug('integer overflow')\n"
    + "     //else if (out_of_bounds_re.exec(e.message)) debug('integer overflow')\n"
    + "     //else if (unrepresentable_re.exec(e.message)) debug('integer overflow')\n"
    + "     //else debug(e.message)\n"
    + "     debug(e)\n"
    + "}"

console.log(program);