const fs = require('fs');

if (process.argv.length !== 4) {
    console.log("Missing filename or javascript engine name argument");
    console.log(" Usage: node convert.js filename enginename");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

const error_int_zero_div = 'integer division by zero';
const error_overflow = 'integer overflow';
const error_unreachable = 'unreachable';
const error_stack = 'stack';
const error_data_segment = 'data segment';

console.log(
`var debug = debug || (arg => console.log('-->', arg));
function debugfl (f) {
    let s = f.toString();
    let slen = s.length;
    if (s.includes('.') && slen > 17) {
	debug(s.slice(0,slen-1));
    } else {
	debug(s);
    }
}
let unrepresentable_re = /(unrepresentable)/i;
`);

switch(process.argv[3]) {
    case "ch":
      console.log(
`let zero_div_re = /(Division by zero)/i;
let overflow_re = /(overflow)/i;
let unreachable_re = /(Unreachable Code)/i;
let stack_re = /(Out of stack space)/i;
let data_segment_re = /(Data segment is out of range)/i;
`);
      break;
    case "jsc":
      console.log(
`let zero_div_re = /(Division by zero)/i;
let overflow_re = /(Out of bounds)/i;
let unreachable_re = /(Unreachable code should not be executed)/i;
let stack_re = /(Maximum call stack size exceeded)/i;
let data_segment_re = /(segment writes outside of memory)/i;
`);
      break;
    case "sm":
      console.log(
`let zero_div_re = /(integer divide by zero)/i;
let overflow_re = /(overflow)/i;
let unreachable_re = /(unreachable executed)/i;
let stack_re = /(too much recursion)/i;
let data_segment_re = /(data segment does not fit in memory)/i;
`);
        break;
    case "v8":
      console.log(
`let zero_div_re = /(by zero)/i;
let overflow_re = /(float unrepresentable in integer range)/i;
let unreachable_re = /(unreachable)/i;
let stack_re = /(Maximum call stack size exceeded)/i;
let data_segment_re = /(data segment is out of bounds)/i;
`);
        break;
  } 

console.log("let importObject = { imports: { log: debug, logfl: debugfl } };");
console.log("let buffer = new Uint8Array(\[", b.toString(), "]);");
console.log(
`
try {
     let m = new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject);
     debug(m.exports.runi32());
     debugfl(m.exports.runf32());
     debugfl(m.exports.runf64());
} catch(e) {
     if (zero_div_re.exec(e.message)) debug('" + error_int_zero_div + "')
     else if (unreachable_re.exec(e.message)) debug('" + error_unreachable + "')
     else if (stack_re.exec(e.message)) debug('" + error_stack + "')
     else if (overflow_re.exec(e.message)) debug('" + error_overflow + "')
     else if (unrepresentable_re.exec(e.message)) debug('" + error_overflow + "')
     else if (data_segment_re.exec(e.message)) debug('" + error_data_segment + "')
     else debug(e)
}
`);
