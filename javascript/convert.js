const fs = require('fs');

if (process.argv.length !== 4) {
    console.log("Missing filename or javascript engine name argument");
    console.log(" Usage: node convert.js filename enginename");
    process.exit(1);
}

const buf = fs.readFileSync(process.argv[2]);
const b = new Uint8Array(buf);

console.log("function printfl (f) { print(f.toString(2)); }");

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
let indirect_re = /(WebAssembly exported function expected)|(Function called with invalid signature)/;
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
let indirect_re = /(call_indirect to a null table entry)|(call_indirect to a signature that does not match)/;
`);
      break;
    case "sm":
      console.log(
`let unrepresentable_re = /(unrepresentable in)/;
let zero_div_re = /(integer divide by zero)/;
let overflow_re = /(invalid conversion to integer)|(integer overflow)/;
let unreachable_re = /(unreachable executed)/;
let stack_re = /(too much recursion)/;
let data_segment_re = /(data segment does not fit in memory)/;
let mem_index_re = /(index out of bounds)/;
let indirect_re = /(indirect call to null)|(indirect call signature mismatch)/;
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
let indirect_re = /(function signature mismatch)/;
`);
        break;
  } 

console.log("let importObject = { imports: { log: printfl, logfl: printfl } };");
console.log("let buffer = new Uint8Array(\[", b.toString(), "]);");
console.log(
`
function protected_run (f) {
  try { f(); } catch(e) {
       let name;

       if (e instanceof WebAssembly.CompileError) name = 'CompileError'
       else if (e instanceof WebAssembly.LinkError) name = 'LinkError'
       else if (e instanceof WebAssembly.RuntimeError) name = 'RuntimeError'
       else name = e.name

       if (zero_div_re.test(e.message))             print(name, 'integer division by zero')
       else if (unreachable_re.test(e.message))     print(name, 'unreachable executed')
       else if (stack_re.test(e.message))           print('stack overflow')
       // v8, jsc: RangeError, sm: InternalError, ch: Error
       else if (overflow_re.test(e.message))        print(name, 'invalid conversion to integer or overflow')
       else if (unrepresentable_re.test(e.message)) print(name, 'unrepresentable')
       else if (data_segment_re.test(e.message))    print(name, 'data segment does not fit memory')
       else if (mem_index_re.test(e.message))       print(name, 'out of bounds memory access')
       else if (indirect_re.test(e.message))   print(name, 'indirect call error')
       else print(name, e)
  }
}

protected_run ( function () {
    let m = new WebAssembly.Instance(new WebAssembly.Module(buffer), importObject);
    for (const [f] of Object.entries(m.exports).sort((a, b) => a[0].localeCompare(b[0]))) {
      print(f);
      protected_run ( () => printfl(m.exports[f]()));
    }
  } )
`);
