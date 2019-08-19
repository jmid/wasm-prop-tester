const fs = require('fs');

if (process.argv.length !== 4) {
    console.log("Missing filename or javascript engine name argument");
    console.log(" Usage: node convert.js filename enginename");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

console.log(
`function printfl (f) {
    let s = f.toString();
    let slen = s.length;
    if (s.includes('.') && slen > 16) {
//	print(s.slice(0,slen-1));
//	print(f.toFixed(12));
	print(f.toFixed(14 - s.indexOf('.')));
    } else {
	print(s);
    }
}
`);

switch(process.argv[3]) {
    case "ch":
      console.log(
`let unrepresentable_re = /(unrepresentable in)/;
let zero_div_re = /(Division by zero)/;
let overflow_re = /(Overflow)/;
let unreachable_re = /(Unreachable Code)/;
let stack_re = /(Out of stack space)/;
let data_segment_re = /(Data segment is out of range)/;
let mem_index_re = /(Memory index is out of range)/;
`);
      break;
    case "jsc":
      console.log(
`let unrepresentable_re = /(unrepresentable in)/;
let zero_div_re = /(Division by zero)/;
let overflow_re = /Out of bounds Trunc operation/;
let unreachable_re = /(Unreachable code should not be executed)/;
let stack_re = /(Maximum call stack size exceeded)/;
let data_segment_re = /(segment writes outside of memory)/;
let mem_index_re = /(Out of bounds memory access)/;
`);
      break;
    case "sm":
      console.log(
`let unrepresentable_re = /(unrepresentable in)/;
let zero_div_re = /(integer divide by zero)/;
let overflow_re = /(integer overflow)/;
let unreachable_re = /(unreachable executed)/;
let stack_re = /(too much recursion)/;
let data_segment_re = /(data segment does not fit in memory)/;
let mem_index_re = /(index out of bounds)/;
`);
        break;
    case "v8":
      console.log(
`let unrepresentable_re = /(unrepresentable in)/;
let zero_div_re = /(divide|remainder) by zero/;
let overflow_re = /(float unrepresentable in integer range)/;
let unreachable_re = /(unreachable)/;
let stack_re = /(Maximum call stack size exceeded)/;
let data_segment_re = /(data segment is out of bounds)/;
let mem_index_re = /(memory access out of bounds)/;
`);
        break;
  } 

console.log("let importObject = { imports: { log: print, logfl: printfl } };");
console.log("let buffer = new Uint8Array(\[", b.toString(), "]);");
console.log(
`
function protected_run (f) {
  try { f(); } catch(e) {
       if (zero_div_re.test(e.message))             print('integer division by zero')
       else if (unreachable_re.test(e.message))     print('unreachable code')
       else if (stack_re.test(e.message))           print('stack overflow')
       else if (overflow_re.test(e.message))        print('overflow')
       else if (unrepresentable_re.test(e.message)) print('unrepresentable')
       else if (data_segment_re.test(e.message))    print('data segment')
       else if (mem_index_re.test(e.message))       print('memory index out of bounds')
       else print(e)
  }
}

protected_run ( function () {
    let m = new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject);
    protected_run ( () => print(m.exports.runi32()) );
    protected_run ( () => printfl(m.exports.runf32()) );
    protected_run ( () => printfl(m.exports.runf64()) );
  } )
`);
